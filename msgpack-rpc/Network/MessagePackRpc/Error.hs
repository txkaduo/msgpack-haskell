{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Network.MessagePackRpc.Error (
  RpcError(..), RemoteError(..)
  ) where

import Control.Exception as E
import Data.Typeable
import Data.MessagePack as M

-- | error detected in my side
data RpcError = ParamError String
                | MethodNotFound String
                | RequestTypeError String
                | ResultTypeError String    -- ^ Result type mismatch
                | ProtocolError String      -- ^ Protocol error
                deriving (Show, Eq, Ord, Typeable)

instance Exception RpcError

-- | error recieved from peer
data RemoteError = RemoteError Object
                deriving (Show, Typeable)
instance Exception RemoteError
