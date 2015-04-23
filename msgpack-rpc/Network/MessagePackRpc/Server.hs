{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-------------------------------------------------------------------
-- |
-- Module    : Network.MessagePackRpc.Server
-- Copyright : (c) Hideyuki Tanaka, 2010-2012
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- This module is server library of MessagePack-RPC.
-- The specification of MessagePack-RPC is at
-- <http://redmine.msgpack.org/projects/msgpack/wiki/RPCProtocolSpec>.
--
-- A simple example:
--
-- > import Network.MessagePackRpc.Server
-- >
-- > add :: Int -> Int -> Method Int
-- > add x y = return $ x + y
-- >
-- > main = serve 1234 [("add", toMethod add)]
--
--------------------------------------------------------------------

module Network.MessagePackRpc.Server (
  -- * RPC method types
  RpcMethod, RpcSink, RpcMethodG, MethodType(..),
  MethodT(..), Method,
  -- * Start RPC server
  serve,
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Data.Conduit
import Data.Conduit.Network
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Attoparsec as CA
import Data.MessagePack
import Network.MessagePackRpc.Types
import Network.MessagePackRpc.Error

-- | Create response to peer request
type RpcMethod m = RpcMethodG Object m

-- | Handler notification from peer, no response will be made.
type RpcSink m = RpcMethodG () m

type RpcMethodG mr m = [Object] -> m mr

type Method = MethodT IO

newtype MethodT m a = MethodT { unMethodT :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans MethodT where
  lift = MethodT

class MethodType f mr m | f -> m where
  -- | Create a RPC method from a Hakell function
  toMethod :: f -> RpcMethodG mr m

instance (Functor m, MonadThrow m, OBJECT o)
         => MethodType (MethodT m o) Object m where
  toMethod m ls = case ls of
    [] -> toObject <$> unMethodT m
    _ -> throwM $ ParamError "too many arguments"

instance (Functor m, MonadThrow m)
         => MethodType (MethodT m ()) () m where
  toMethod m ls = case ls of
    [] -> unMethodT m
    _ -> throwM $ ParamError "too many arguments"

instance (OBJECT o, MethodType r mr m, MonadThrow m) => MethodType (o -> r) mr m where
  toMethod f = go
    where
      go (x:xs)   = (either (throwM . ParamError) return $ tryFromObject x)
                        >>= flip toMethod xs . f
      go []       = throwM $ ParamError "too few arguments"

-- | Start RPC server with a set of RPC methods.
-- Caution: Does not support 'notification'.
serve :: forall m . (MonadIO m, MonadThrow m)
         =>
         (forall b. m b -> IO b)
         -> Int                     -- ^ Port number
         -> [(String, RpcMethod m)] -- ^ list of (method name, RPC method)
         -> m ()
serve run_in_io port methods = liftIO $ runTCPServer (serverSettings port "*") $ \ad -> void $ run_in_io $ do
  (rsrc, _) <- appSource ad $$+ return ()
  processRequests rsrc (appSink ad)
  where
    processRequests rsrc sink = do
      (rsrc', res) <- rsrc $$++ do
        req <- CA.sinkParser get
        lift $ getResponse req
      _ <- CB.sourceLbs (pack res) $$ sink
      processRequests rsrc' sink

    getResponse :: Request -> m Response
    getResponse (rtype, msgid, methodName, args) = do
      when (rtype /= 0) $
        throwM $ RequestTypeError $ "request type is not 0, got " ++ show rtype
      ret <- callMethod methodName args
      return (1, msgid, toObject (), ret)

    callMethod :: String -> [Object] -> m Object
    callMethod methodName args =
      case lookup methodName methods of
        Nothing ->
          throwM $ MethodNotFound $ "method '" ++ methodName ++ "' not found"
        Just method ->
          method args
