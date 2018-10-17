{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Raft.Handle where

import Protolude

import Control.Monad.Writer.Strict

import Data.Type.Bool
import Data.Monoid
import qualified Debug.Trace as DT

import qualified Raft.Follower as Follower
import qualified Raft.Candidate as Candidate
import qualified Raft.Leader as Leader

import Raft.Action
import Raft.Config
import Raft.Event
import Raft.Monad
import Raft.NodeState
import Raft.Persistent
import Raft.RPC
import Raft.Types

-- | Main entry point for handling events
handleEvent
  :: forall v. Show v
  => NodeConfig
  -> RaftNodeState v
  -> PersistentState v
  -> Event v
  -> (RaftNodeState v, PersistentState v, TransitionWriter v)
handleEvent nodeConfig raftNodeState@(RaftNodeState initNodeState) persistentState event =
    -- Rules for all servers:
    case handleNewerRPCTerm of
      (RaftNodeState resNodeState, persistentState', transitionW) ->
        case handleEvent' (raftHandler resNodeState) nodeConfig resNodeState persistentState' event of
          (ResultState _ resultState, persistentState'', transitionW') ->
            (RaftNodeState resultState, persistentState'', transitionW <> transitionW')
  where
    raftHandler :: forall s. NodeState s -> RaftHandler s v
    raftHandler nodeState =
      case nodeState of
        NodeFollowerState _ -> followerRaftHandler
        NodeCandidateState _ -> candidateRaftHandler
        NodeLeaderState _ -> leaderRaftHandler

    handleNewerRPCTerm :: (RaftNodeState v, PersistentState v, TransitionWriter v)
    handleNewerRPCTerm =
      case event of
        Message (RPC _ rpc) ->
          runTransitionM nodeConfig persistentState $ do
            -- If RPC request or response contains term T > currentTerm: set
            -- currentTerm = T, convert to follower
            currentTerm <- gets psCurrentTerm
            if rpcTerm rpc > currentTerm
              then
                case convertToFollower initNodeState of
                  ResultState _ nodeState -> do
                    modify $ \pstate ->
                      pstate { psCurrentTerm = rpcTerm rpc
                             , psVotedFor = Nothing
                             }
                    resetElectionTimeout
                    pure (RaftNodeState nodeState)
              else pure raftNodeState
        _ -> (raftNodeState, persistentState, mempty)

    convertToFollower :: NodeState s -> ResultState s v
    convertToFollower nodeState =
      case nodeState of
        NodeFollowerState _ ->
          ResultState HigherTermFoundFollower nodeState
        NodeCandidateState cs ->
          ResultState HigherTermFoundCandidate $
            NodeFollowerState FollowerState
              { fsCurrentLeader = NoLeader
              , fsCommitIndex = csCommitIndex cs
              , fsLastApplied = csLastApplied cs
              }
        NodeLeaderState ls ->
          ResultState HigherTermFoundLeader $
            NodeFollowerState FollowerState
              { fsCurrentLeader = NoLeader
              , fsCommitIndex = lsCommitIndex ls
              , fsLastApplied = lsLastApplied ls
              }


data RaftHandler s v = RaftHandler
  { handleAppendEntries :: RPCHandler s (AppendEntries v) v
  , handleAppendEntriesResponse :: RPCHandler s AppendEntriesResponse v
  , handleRequestVote :: RPCHandler s RequestVote v
  , handleRequestVoteResponse :: RPCHandler s RequestVoteResponse v
  , handleTimeout :: TimeoutHandler s v
  , handleClientRequest :: ClientReqHandler s v
  }

followerRaftHandler :: RaftHandler 'Follower v
followerRaftHandler = RaftHandler
  { handleAppendEntries = Follower.handleAppendEntries
  , handleAppendEntriesResponse = Follower.handleAppendEntriesResponse
  , handleRequestVote = Follower.handleRequestVote
  , handleRequestVoteResponse = Follower.handleRequestVoteResponse
  , handleTimeout = Follower.handleTimeout
  , handleClientRequest = Follower.handleClientRequest
  }

candidateRaftHandler :: RaftHandler 'Candidate v
candidateRaftHandler = RaftHandler
  { handleAppendEntries = Candidate.handleAppendEntries
  , handleAppendEntriesResponse = Candidate.handleAppendEntriesResponse
  , handleRequestVote = Candidate.handleRequestVote
  , handleRequestVoteResponse = Candidate.handleRequestVoteResponse
  , handleTimeout = Candidate.handleTimeout
  , handleClientRequest = Candidate.handleClientRequest
  }

leaderRaftHandler :: RaftHandler 'Leader v
leaderRaftHandler = RaftHandler
  { handleAppendEntries = Leader.handleAppendEntries
  , handleAppendEntriesResponse = Leader.handleAppendEntriesResponse
  , handleRequestVote = Leader.handleRequestVote
  , handleRequestVoteResponse = Leader.handleRequestVoteResponse
  , handleTimeout = Leader.handleTimeout
  , handleClientRequest = Leader.handleClientRequest
  }

handleEvent'
  :: forall s v. Show v
  => RaftHandler s v
  -> NodeConfig
  -> NodeState s
  -> PersistentState v
  -> Event v
  -> (ResultState s v, PersistentState v, TransitionWriter v)
handleEvent' raftHandler@RaftHandler{..} nodeConfig initNodeState persistentState event =
    runTransitionM nodeConfig persistentState $ do
      case event of
        Message msg -> handleMessage msg
        ClientWriteRequest crq -> do
          --tellLogWithState initNodeState (show crq)
          handleClientRequest initNodeState crq
        Timeout tout -> do
          tellLogWithState initNodeState (show tout)
          handleTimeout initNodeState tout
        ClientReadRequest _ -> panic "No read requests here"
  where
    handleMessage :: Message v -> TransitionM v (ResultState s v)
    handleMessage (RPC sender rpc) = do
      resState@(ResultState transition newNodeState) <- case rpc of
        AppendEntriesRPC appendEntries -> do
          tellLogWithState initNodeState (show appendEntries)
          handleAppendEntries initNodeState sender appendEntries
        AppendEntriesResponseRPC appendEntriesResp -> do
          tellLogWithState initNodeState (show appendEntriesResp)
          handleAppendEntriesResponse initNodeState sender appendEntriesResp
        RequestVoteRPC requestVote -> do
          tellLogWithState initNodeState (show requestVote)
          handleRequestVote initNodeState sender requestVote
        RequestVoteResponseRPC requestVoteResp -> do
          tellLogWithState initNodeState (show requestVoteResp)
          handleRequestVoteResponse initNodeState sender requestVoteResp

      -- If commitIndex > lastApplied: increment lastApplied, apply
      -- log[lastApplied] to state machine (Section 5.3)
      newestNodeState <-
        if commitIndex newNodeState > lastApplied newNodeState
          then incrLastApplied newNodeState
          else pure newNodeState

      pure $ ResultState transition newestNodeState

    incrLastApplied :: NodeState s' -> TransitionM v (NodeState s')
    incrLastApplied nodeState =
      case nodeState of
        NodeFollowerState fs -> do
          applyLogEntry (lastApplied nodeState)
          let lastApplied' = incrIndex (fsLastApplied fs)
          pure $ NodeFollowerState $
            fs { fsLastApplied = lastApplied' }
        NodeCandidateState cs -> do
          applyLogEntry (lastApplied nodeState)
          let lastApplied' = incrIndex (csLastApplied cs)
          pure $ NodeCandidateState $
            cs { csLastApplied = lastApplied' }
        NodeLeaderState ls -> do
          applyLogEntry (lastApplied nodeState)
          let lastApplied' = incrIndex (lsLastApplied ls)
          pure $ NodeLeaderState $
            ls { lsLastApplied = lastApplied' }

    getLastAppliedAndCommitIndex :: NodeState s' -> (Index, Index)
    getLastAppliedAndCommitIndex nodeState =
      case nodeState of
        NodeFollowerState fs -> (fsLastApplied fs, fsCommitIndex fs)
        NodeCandidateState cs -> (csLastApplied cs, csCommitIndex cs)
        NodeLeaderState ls -> (lsLastApplied ls, lsCommitIndex ls)

    lastApplied :: NodeState s' -> Index
    lastApplied = fst . getLastAppliedAndCommitIndex

    commitIndex :: NodeState s' -> Index
    commitIndex = snd . getLastAppliedAndCommitIndex
