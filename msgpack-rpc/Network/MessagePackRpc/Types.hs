module Network.MessagePackRpc.Types (
  -- * RPC Call Types
  RpcMessageId, RpcMessageType,
  Request, Notification, Response,
  RpcInPacket(..),
  ) where

import Data.Bits
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
                    | RpcInResponse Response
                    deriving (Show, Eq)

instance Packable RpcInPacket where
    from (RpcInRequest x)       = from x
    from (RpcInNotification x)  = from x
    from (RpcInResponse x)      = from x

instance Unpackable RpcInPacket where
    get = get_re <|> get_notification
        where
            get_re = do
                (rtype, msgid, obj3, obj4) <- get
                if rtype .&. 1 == 0
                    then do
                        name <- either fail return $ tryFromObject obj3
                        args <- if obj4 == ObjectNil
                                    then return []
                                    else either fail return $ tryFromObject obj4
                        return $ RpcInRequest (rtype, msgid, name, args)
                    else
                        return $ RpcInResponse (rtype, msgid, obj3, obj4)

            get_notification = RpcInNotification <$> get

instance OBJECT RpcInPacket where
    toObject (RpcInRequest x)       = toObject x
    toObject (RpcInNotification x)  = toObject x
    toObject (RpcInResponse x)      = toObject x

