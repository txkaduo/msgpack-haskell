{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-# LANGUAGE GADTs, FlexibleContexts, RankNTypes, FlexibleInstances #-}

-------------------------------------------------------------------
-- |
-- Module    : Network.MessagePackRpc.Client
-- Copyright : (c) Hideyuki Tanaka, 2010-2012
-- License   : BSD3
--
-- Maintainer:  Hideyuki Tanaka <tanaka.hideyuki@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
-- This module is client library of MessagePack-RPC.
-- The specification of MessagePack-RPC is at
-- <http://redmine.msgpack.org/projects/msgpack/wiki/RPCProtocolSpec>.
--
-- A simple example:
--
-- > import Network.MessagePackRpc.Client
-- >
-- > add :: Int -> Int -> Client Int
-- > add = call "add"
-- >
-- > main = runClient "localhost" 5000 $ do
-- >   ret <- add 123 456
-- >   liftIO $ print ret
--
--------------------------------------------------------------------

module Network.MessagePackRpc.Client (
  -- * MessagePack Client type
  ClientT, Client,
  runClient,

  RpcType, rpcc,
  RpcNotify, rpcn,

  -- * Call RPC method
  call,
  notify,

  -- * RPC error
  RpcError(..),
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State.Strict as CMS
import qualified Data.ByteString as S
import Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Network
import Data.MessagePack as M
-- import Data.Typeable

import Network.MessagePackRpc.Error

type Client = ClientT IO

newtype ClientT m a
  = ClientT { unClientT :: StateT (Connection m) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadThrow)

-- It can't derive by newtype deriving...
instance MonadTrans ClientT where
  lift = ClientT . lift

-- | RPC connection type
data Connection m where
  Connection ::
    !(ResumableSource m S.ByteString)
    -> !(Sink S.ByteString m ())
    -> !Int
    -> Connection m

runClient :: (MonadIO m, Functor m) =>
    (forall b. m b -> IO b)
    -> S.ByteString -> Int -> ClientT m a -> m ()
runClient run_in_io host port m = do
  liftIO $ do
    runTCPClient (clientSettings port host) $ \ad -> run_in_io $ do
      (rsrc, _) <- appSource ad $$+ return ()
      void $ evalStateT (unClientT m) (Connection rsrc (appSink ad) 0)


class RpcNotify r where
  rpcn :: String -> [Object] -> r

instance (MonadIO m, MonadThrow m) => RpcNotify (ClientT m ()) where
  rpcn m args = do
    rpcNotify m (reverse args)

instance (OBJECT o, RpcNotify r) => RpcNotify (o -> r) where
  rpcn m args arg = rpcn m (toObject arg:args)

class RpcType r where
  rpcc :: String -> [Object] -> r

instance (MonadIO m, MonadThrow m, OBJECT o) => RpcType (ClientT m o) where
  rpcc m args = do
    res <- rpcCall m (reverse args)
    case tryFromObject res of
      Left err -> throwM $ ResultTypeError err
      Right r  -> return r

instance (OBJECT o, RpcType r) => RpcType (o -> r) where
  rpcc m args arg = rpcc m (toObject arg:args)

rpcCall :: (MonadIO m, MonadThrow m) => String -> [Object] -> ClientT m Object
rpcCall methodName args = ClientT $ do
  Connection rsrc sink msgid <- CMS.get
  (rsrc', (rtype, rmsgid, rerror, rresult)) <- lift $ do
    CB.sourceLbs (pack (0 :: Int, msgid, methodName, args)) $$ sink
    rsrc $$++ CA.sinkParser M.get
  CMS.put $ Connection rsrc' sink (msgid + 1)

  when (rtype /= (1 :: Int)) $
    throwM $ ProtocolError $
      "invalid response type (expect 1, but got " ++ show rtype ++ ")"
  when (rmsgid /= msgid) $
    throwM $ ProtocolError $
      "message id mismatch: expect "
      ++ show msgid ++ ", but got "
      ++ show rmsgid
  case tryFromObject rerror of
    Left _ ->
      throwM $ RemoteError rerror
    Right () ->
      return rresult

rpcNotify :: (MonadIO m, MonadThrow m) => String -> [Object] -> ClientT m ()
rpcNotify methodName args = ClientT $ do
  Connection _rsrc sink _msgid <- CMS.get
  lift $ do
    CB.sourceLbs (pack (2 :: Int, methodName, args)) $$ sink

-- | Call an RPC Method
call :: RpcType a
        => String -- ^ Method name
        -> a
call m = rpcc m []

-- | Send notification
notify :: RpcNotify a
        => String -- ^ Method name
        -> a
notify m = rpcn m []
