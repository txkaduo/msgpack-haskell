{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, IncoherentInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Object
-- Copyright : (c) Hideyuki Tanaka, 2009-2011
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- MessagePack object definition
--
--------------------------------------------------------------------

module Data.MessagePack.Object(
  -- * MessagePack Object
  Object(..),
  
  -- * Serialization to and from Object
  OBJECT(..),
  -- Result,
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vector as V
import Data.Typeable
import Data.Int

import Data.MessagePack.Assoc
import Data.MessagePack.Pack
import Data.MessagePack.Unpack
import Data.MessagePack.Internal.Utf8

-- | Object Representation of MessagePack data.
data Object
  = ObjectNil
  | ObjectBool Bool
  | ObjectInteger Integer
  | ObjectFloat Float
  | ObjectDouble Double
  | ObjectRAW B.ByteString
  | ObjectText T.Text
  | ObjectArray [Object]
  | ObjectMap [(Object, Object)]
  deriving (Show, Eq, Ord, Typeable)  

showObjectType :: Object -> String
showObjectType ObjectNil            = "Nil"
showObjectType (ObjectBool {})      = "Bool"
showObjectType (ObjectInteger {})   = "Integer"
showObjectType (ObjectFloat {})     = "Float"
showObjectType (ObjectDouble {})    = "Double"
showObjectType (ObjectRAW {})       = "RAW"
showObjectType (ObjectText {})      = "Text"
showObjectType (ObjectArray {})     = "Array"
showObjectType (ObjectMap {})       = "Map"

instance NFData Object where
  rnf obj =
    case obj of
      ObjectNil -> ()
      ObjectBool b -> rnf b
      ObjectInteger n -> rnf n
      ObjectFloat f -> rnf f
      ObjectDouble d -> rnf d
      ObjectRAW bs -> bs `seq` ()
      ObjectText t -> rnf t
      ObjectArray a -> rnf a
      ObjectMap m -> rnf m

instance Unpackable Object where
  get =
    A.choice
    [ liftM ObjectInteger get
    , liftM (\() -> ObjectNil) get
    , liftM ObjectBool get
    , liftM ObjectFloat get
    , liftM ObjectDouble get
    , liftM ObjectRAW get
    , liftM ObjectText get
    , liftM ObjectArray get
    , liftM (ObjectMap . unAssoc) get
    ]

instance Packable Object where
  from obj =
    case obj of
      ObjectInteger n ->
        from n
      ObjectNil ->
        from ()
      ObjectBool b ->
        from b
      ObjectFloat f ->
        from f
      ObjectDouble d ->
        from d
      ObjectRAW raw ->
        from raw
      ObjectText t ->
        from t
      ObjectArray arr ->
        from arr
      ObjectMap m ->
        from $ Assoc m

-- | The class of types serializable to and from MessagePack object
class (Unpackable a, Packable a) => OBJECT a where
  -- | Encode a value to MessagePack object
  toObject :: a -> Object
  toObject = unpack . pack
  
  -- | Decode a value from MessagePack object
  fromObject :: Object -> a
  fromObject a =
    case tryFromObject a of
      Left err ->
        throw $ UnpackError err
      Right ret ->
        ret

  -- | Decode a value from MessagePack object
  tryFromObject :: Object -> Either String a
  tryFromObject = tryUnpack . pack

instance OBJECT Object where
  toObject = id
  tryFromObject = Right

tryFromObjectError :: String -> Object -> Either String a
tryFromObjectError expected x = Left $ "tryFromObject: cannot cast from "
                                            ++ showObjectType x ++ " to " ++ expected

instance OBJECT () where
  toObject = const ObjectNil
  tryFromObject ObjectNil = Right ()
  tryFromObject x = tryFromObjectError "()" x

instance OBJECT Int where
  toObject = ObjectInteger . fromIntegral
  tryFromObject (ObjectInteger n) = Right $ fromIntegral n
  tryFromObject x = tryFromObjectError "Int" x

instance OBJECT Int64 where
  toObject = ObjectInteger . fromIntegral
  tryFromObject (ObjectInteger n) = Right $ fromIntegral n
  tryFromObject x = tryFromObjectError "Int64" x

instance OBJECT Integer where
  toObject = ObjectInteger
  tryFromObject (ObjectInteger n) = Right n
  tryFromObject x = tryFromObjectError "Integer" x

instance OBJECT Bool where
  toObject = ObjectBool
  tryFromObject (ObjectBool b) = Right b
  tryFromObject x = tryFromObjectError "Bool" x

instance OBJECT Double where
  toObject = ObjectDouble
  tryFromObject (ObjectDouble d) = Right d
  tryFromObject x = tryFromObjectError "Double" x

instance OBJECT Float where
  toObject = ObjectFloat
  tryFromObject (ObjectFloat f) = Right f
  tryFromObject x = tryFromObjectError "Float" x

instance OBJECT String where
  toObject = toObject . T.pack
  tryFromObject obj = liftM T.unpack $ tryFromObject obj

instance OBJECT B.ByteString where
  toObject = ObjectRAW
  tryFromObject (ObjectRAW bs) = Right bs
  tryFromObject x = tryFromObjectError "ByteString" x

instance OBJECT BL.ByteString where
  toObject = ObjectRAW . fromLBS
  tryFromObject (ObjectRAW bs) = Right $ toLBS bs
  tryFromObject x = tryFromObjectError "ByteString" x

instance OBJECT T.Text where
  toObject = ObjectText
  tryFromObject (ObjectRAW bs) = Right $ T.decodeUtf8With skipChar bs
  tryFromObject (ObjectText t) = Right t
  tryFromObject x = tryFromObjectError "Text" x

instance OBJECT TL.Text where
  toObject = ObjectText . TL.toStrict
  tryFromObject (ObjectRAW bs) = Right $ TL.decodeUtf8With skipChar $ toLBS bs
  tryFromObject (ObjectText t) = Right $ TL.fromStrict t
  tryFromObject x = tryFromObjectError "Text" x

instance OBJECT a => OBJECT [a] where
  toObject = ObjectArray . map toObject
  tryFromObject (ObjectArray arr) =
    mapM tryFromObject arr
  tryFromObject x =
    tryFromObjectError "[a]" x

instance (OBJECT a1, OBJECT a2) => OBJECT (a1, a2) where
  toObject (a1, a2) = ObjectArray [toObject a1, toObject a2]
  tryFromObject x@(ObjectArray arr) =
    case arr of
      [o1, o2] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        return (v1, v2)
      _ ->
        tryFromObjectError "(a1, a2)" x
  tryFromObject x =
    tryFromObjectError "(a1, a2)" x

instance (OBJECT a1, OBJECT a2, OBJECT a3) => OBJECT (a1, a2, a3) where
  toObject (a1, a2, a3) = ObjectArray [toObject a1, toObject a2, toObject a3]
  tryFromObject x@(ObjectArray arr) =
    case arr of
      [o1, o2, o3] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        v3 <- tryFromObject o3
        return (v1, v2, v3)
      _ ->
        tryFromObjectError "(a1, a2, a3)" x
  tryFromObject x =
    tryFromObjectError "(a1, a2, a3)" x

instance (OBJECT a1, OBJECT a2, OBJECT a3, OBJECT a4) => OBJECT (a1, a2, a3, a4) where
  toObject (a1, a2, a3, a4) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4]
  tryFromObject x@(ObjectArray arr) =
    case arr of
      [o1, o2, o3, o4] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        v3 <- tryFromObject o3
        v4 <- tryFromObject o4
        return (v1, v2, v3, v4)
      _ ->
        tryFromObjectError "(a1, a2, a3, a4)" x
  tryFromObject x =
    tryFromObjectError "(a1, a2, a3, a4)" x

instance (OBJECT a1, OBJECT a2, OBJECT a3, OBJECT a4, OBJECT a5) => OBJECT (a1, a2, a3, a4, a5) where
  toObject (a1, a2, a3, a4, a5) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5]
  tryFromObject x@(ObjectArray arr) =
    case arr of
      [o1, o2, o3, o4, o5] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        v3 <- tryFromObject o3
        v4 <- tryFromObject o4
        v5 <- tryFromObject o5
        return (v1, v2, v3, v4, v5)
      _ ->
        tryFromObjectError "(a1, a2, a3, a4, a5)" x
  tryFromObject x =
    tryFromObjectError "(a1, a2, a3, a4, a5)" x

instance (OBJECT a1, OBJECT a2, OBJECT a3, OBJECT a4, OBJECT a5, OBJECT a6) => OBJECT (a1, a2, a3, a4, a5, a6) where
  toObject (a1, a2, a3, a4, a5, a6) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6]
  tryFromObject x@(ObjectArray arr) =
    case arr of
      [o1, o2, o3, o4, o5, o6] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        v3 <- tryFromObject o3
        v4 <- tryFromObject o4
        v5 <- tryFromObject o5
        v6 <- tryFromObject o6
        return (v1, v2, v3, v4, v5, v6)
      _ ->
        tryFromObjectError "(a1, a2, a3, a4, a5, a6)" x
  tryFromObject x =
    tryFromObjectError "(a1, a2, a3, a4, a5, a6)" x

instance (OBJECT a1, OBJECT a2, OBJECT a3, OBJECT a4, OBJECT a5, OBJECT a6, OBJECT a7) => OBJECT (a1, a2, a3, a4, a5, a6, a7) where
  toObject (a1, a2, a3, a4, a5, a6, a7) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7]
  tryFromObject x@(ObjectArray arr) =
    case arr of
      [o1, o2, o3, o4, o5, o6, o7] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        v3 <- tryFromObject o3
        v4 <- tryFromObject o4
        v5 <- tryFromObject o5
        v6 <- tryFromObject o6
        v7 <- tryFromObject o7
        return (v1, v2, v3, v4, v5, v6, v7)
      _ ->
        tryFromObjectError "(a1, a2, a3, a4, a5, a6, a7)" x
  tryFromObject x =
    tryFromObjectError "(a1, a2, a3, a4, a5, a6, a7)" x

instance (OBJECT a1, OBJECT a2, OBJECT a3, OBJECT a4, OBJECT a5, OBJECT a6, OBJECT a7, OBJECT a8) => OBJECT (a1, a2, a3, a4, a5, a6, a7, a8) where
  toObject (a1, a2, a3, a4, a5, a6, a7, a8) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7, toObject a8]
  tryFromObject x@(ObjectArray arr) =
    case arr of
      [o1, o2, o3, o4, o5, o6, o7, o8] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        v3 <- tryFromObject o3
        v4 <- tryFromObject o4
        v5 <- tryFromObject o5
        v6 <- tryFromObject o6
        v7 <- tryFromObject o7
        v8 <- tryFromObject o8
        return (v1, v2, v3, v4, v5, v6, v7, v8)
      _ ->
        tryFromObjectError "(a1, a2, a3, a4, a5, a6, a7, a8)" x
  tryFromObject x =
    tryFromObjectError "(a1, a2, a3, a4, a5, a6, a7, a8)" x

instance (OBJECT a1, OBJECT a2, OBJECT a3, OBJECT a4, OBJECT a5, OBJECT a6, OBJECT a7, OBJECT a8, OBJECT a9) => OBJECT (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  toObject (a1, a2, a3, a4, a5, a6, a7, a8, a9) = ObjectArray [toObject a1, toObject a2, toObject a3, toObject a4, toObject a5, toObject a6, toObject a7, toObject a8, toObject a9]
  tryFromObject x@(ObjectArray arr) =
    case arr of
      [o1, o2, o3, o4, o5, o6, o7, o8, o9] -> do
        v1 <- tryFromObject o1
        v2 <- tryFromObject o2
        v3 <- tryFromObject o3
        v4 <- tryFromObject o4
        v5 <- tryFromObject o5
        v6 <- tryFromObject o6
        v7 <- tryFromObject o7
        v8 <- tryFromObject o8
        v9 <- tryFromObject o9
        return (v1, v2, v3, v4, v5, v6, v7, v8, v9)
      _ ->
        tryFromObjectError "(a1, a2, a3, a4, a5, a6, a7, a8, a9)" x
  tryFromObject x =
    tryFromObjectError "(a1, a2, a3, a4, a5, a6, a7, a8, a9)" x

instance (OBJECT a, OBJECT b) => OBJECT (Assoc [(a,b)]) where
  toObject =
    ObjectMap . map (\(a, b) -> (toObject a, toObject b)) . unAssoc
  tryFromObject (ObjectMap mem) = do
    Assoc <$> mapM (\(a, b) -> liftM2 (,) (tryFromObject a) (tryFromObject b)) mem
  tryFromObject x =
    tryFromObjectError "(Assoc [(a,b)])" x

instance (OBJECT a, OBJECT b) => OBJECT (Assoc (V.Vector (a,b))) where
  toObject =
    ObjectMap . V.toList . V.map (\(a, b) -> (toObject a, toObject b)) . unAssoc
  tryFromObject (ObjectMap mem) = do
    Assoc <$> V.mapM (\(a, b) -> liftM2 (,) (tryFromObject a) (tryFromObject b)) (V.fromList mem)
  tryFromObject x =
    tryFromObjectError "(Assoc (V.Vector (a,b)))" x

instance (Ord a, OBJECT a, OBJECT b) => OBJECT (M.Map a b) where
  toObject =
    ObjectMap . map (\(a, b) -> (toObject a, toObject b)) . M.toList
  tryFromObject (ObjectMap mem) = do
    M.fromList <$> mapM (\(a, b) -> liftM2 (,) (tryFromObject a) (tryFromObject b)) mem
  tryFromObject x =
    tryFromObjectError "(M.Map a b)" x

instance OBJECT b => OBJECT (IM.IntMap b) where
  toObject =
    ObjectMap . map (\(a, b) -> (toObject a, toObject b)) . IM.toList
  tryFromObject (ObjectMap mem) = do
    IM.fromList <$> mapM (\(a, b) -> liftM2 (,) (tryFromObject a) (tryFromObject b)) mem
  tryFromObject x =
    tryFromObjectError "(IM.IntMap b)" x

instance (Hashable a, Eq a, OBJECT a, OBJECT b) => OBJECT (HM.HashMap a b) where
  toObject =
    ObjectMap . map (\(a, b) -> (toObject a, toObject b)) . HM.toList
  tryFromObject (ObjectMap mem) = do
    HM.fromList <$> mapM (\(a, b) -> liftM2 (,) (tryFromObject a) (tryFromObject b)) mem
  tryFromObject x =
    tryFromObjectError "(HM.HashMap a b)" x

instance OBJECT a => OBJECT (Maybe a) where
  toObject (Just a) = toObject a
  toObject Nothing = ObjectNil
  
  tryFromObject ObjectNil = return Nothing
  tryFromObject obj = liftM Just $ tryFromObject obj
