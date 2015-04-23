module Network.MessagePackRpc.Types (
  -- * RPC Call Types
  RpcMessageId, RpcMessageType,
  Request, Notification, Response,
  RpcInPacket(..),
  ) where

import Control.Applicative
import Data.MessagePack

-- | use a integer to represent type of message
-- 0 : request
-- 1 : response
-- 2 : notification
type RpcMessageType = Int

type RpcMessageId = Int

type Response = (RpcMessageType, RpcMessageId, Object, Object)

type Request  = (RpcMessageType, RpcMessageId, String, [Object])
type Notification  = (RpcMessageType, String, [Object])

-- | Incoming data from peer
data RpcInPacket =  RpcInRequest Request
                    | RpcInNotification Notification
                    deriving (Show, Eq)

instance Packable RpcInPacket where
    from (RpcInRequest x)       = from x
    from (RpcInNotification x)  = from x

instance Unpackable RpcInPacket where
    get = get_request <|> get_notification
        where
            get_request = RpcInRequest <$> get
            get_notification = RpcInNotification <$> get

instance OBJECT RpcInPacket where
    toObject (RpcInRequest x)       = toObject x
    toObject (RpcInNotification x)  = toObject x

