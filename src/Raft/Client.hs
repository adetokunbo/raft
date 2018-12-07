{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Raft.Client where

import Protolude

import qualified Data.Serialize as S

import Raft.Log (Entry, EntryHash, Entries)
import Raft.Types

-- | Interface for Raft nodes to send messages to clients
class RaftSendClient m sm where
  sendClient :: ClientId -> ClientResponse sm v -> m ()

-- | Interface for Raft nodes to receive messages from clients
class Show (RaftRecvClientError m v) => RaftRecvClient m v where
  type RaftRecvClientError m v
  receiveClient :: m (Either (RaftRecvClientError m v) (ClientRequest v))

--------------------------------------------------------------------------------
-- Client Requests
--------------------------------------------------------------------------------

-- | Representation of a client request coupled with the client id
data ClientRequest v
  = ClientRequest ClientId (ClientReq v)
  deriving (Show, Generic)

instance S.Serialize v => S.Serialize (ClientRequest v)

-- | Representation of a client request
data ClientReq v
  = ClientReadReq ClientReadReq -- ^ Request the latest state of the state machine
  | ClientWriteReq v -- ^ Write a command
  deriving (Show, Generic)

instance S.Serialize v => S.Serialize (ClientReq v)

data ClientReadReq
  = ClientReadEntries ReadEntriesSpec
  | ClientReadStateMachine
  deriving (Show, Generic, S.Serialize)

data ReadEntriesSpec
  = ByIndex Index
  | ByIndices (Maybe Index) (Maybe Index)
  | ByHash EntryHash
  | ByHashes (Set EntryHash)
  deriving (Show, Generic, S.Serialize)

data ReadEntriesError
  = EntryDoesNotExist (Either EntryHash Index)
  | InvalidIntervalSpecified (Index, Index)

--------------------------------------------------------------------------------
-- Client Responses
--------------------------------------------------------------------------------

-- | Specification for the data inside a ClientResponse
data ClientRespSpec
  = ClientReadRespSpec ClientReadRespSpec
  | ClientWriteRespSpec Index
  | ClientRedirRespSpec CurrentLeader
  deriving (Show, Generic, S.Serialize)

data ClientReadRespSpec
  = ClientReadRespSpecEntries ReadEntriesSpec
  | ClientReadRespSpecStateMachine
  deriving (Show, Generic, S.Serialize)

--------------------------------------------------------------------------------

-- | The datatype sent back to the client as an actual response
data ClientResponse sm v
  = ClientReadResponse (ClientReadResp sm v)
    -- ^ Respond with the latest state of the state machine.
  | ClientWriteResponse Index
    -- ^ Respond with the index of the entry appended to the log
  | ClientRedirectResponse CurrentLeader
    -- ^ Respond with the node id of the current leader
  deriving (Show, Generic)

instance (S.Serialize sm, S.Serialize v) => S.Serialize (ClientResponse sm v)

-- | Representation of a read response to a client
data ClientReadResp sm v
  = ClientReadRespStateMachine sm
  | ClientReadRespEntry (Entry v)
  | ClientReadRespEntries (Entries v)
  deriving (Show, Generic)

instance (S.Serialize sm, S.Serialize v) => S.Serialize (ClientReadResp sm v)
