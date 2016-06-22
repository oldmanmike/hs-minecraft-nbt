module Data.NBT.Encode
  ( encodeNBT
  , encodeText
  , encodeByteArray
  , encodeList
  , encodeCompound
  , encodeIntArray
  ) where

import qualified  Data.Array.IArray as A
import qualified  Data.Array.Unboxed as AU
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Builder as Encode
import            Data.Int
import            Data.List
import            Data.Monoid
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.Word

import            Data.NBT.Types

encodeNBT :: NBT -> Encode.Builder
encodeNBT (TagByte name payload) =
  Encode.word8 0x01
  <> encodeText name
  <> Encode.int8 payload

encodeNBT (TagShort name payload) =
  Encode.word8 0x02
  <> encodeText name
  <> Encode.int16BE payload

encodeNBT (TagInt name payload) =
  Encode.word8 0x03
  <> encodeText name
  <> Encode.int32BE payload

encodeNBT (TagLong name payload) =
  Encode.word8 0x04
  <> encodeText name
  <> Encode.int64BE payload

encodeNBT (TagFloat name payload) =
  Encode.word8 0x05
  <> encodeText name
  <> Encode.floatBE payload

encodeNBT (TagDouble name payload) =
  Encode.word8 0x06
  <> encodeText name
  <> Encode.doubleBE payload

encodeNBT (TagByteArray name payload) =
  Encode.word8 0x07
  <> encodeText name
  <> encodeByteArray payload

encodeNBT (TagString name payload) =
  Encode.word8 0x08
  <> encodeText name
  <> encodeText payload

encodeNBT (TagList name payload) =
  Encode.word8 0x09
  <> encodeText name
  <> encodeList payload

encodeNBT (TagCompound name payload) =
  Encode.word8 0x0a
  <> encodeText name
  <> encodeCompound payload

encodeNBT (TagIntArray name payload) =
  Encode.word8 0x0b
  <> encodeText name
  <> encodeIntArray payload

encodeNBT' :: NamelessNBT -> Encode.Builder
encodeNBT' (NTagByte payload)       = Encode.int8 payload
encodeNBT' (NTagShort payload)      = Encode.int16BE payload
encodeNBT' (NTagInt payload)        = Encode.int32BE payload
encodeNBT' (NTagLong payload)       = Encode.int64BE payload
encodeNBT' (NTagFloat payload)      = Encode.floatBE payload
encodeNBT' (NTagDouble payload)     = Encode.doubleBE payload
encodeNBT' (NTagByteArray payload)  = encodeByteArray payload
encodeNBT' (NTagString payload)     = encodeText payload
encodeNBT' (NTagList payload)       = encodeList payload
encodeNBT' (NTagCompound payload)   = encodeCompound payload
encodeNBT' (NTagIntArray payload)   = encodeIntArray payload

encodeByteArray :: (AU.UArray Int32 Int8) -> Encode.Builder
encodeByteArray payload =
  (Encode.int32BE $ (\(a,b) -> toEnum . fromEnum $ AU.rangeSize (a,b)) $ (AU.bounds payload))
  <> (foldl' (<>) mempty (fmap Encode.int8 (AU.elems payload)))

encodeText :: T.Text -> Encode.Builder
encodeText t =
    (Encode.int16BE . toEnum . B.length $ t')
    <> Encode.byteString t'
  where
  t' = encodeUtf8 t

encodeList :: NBTList -> Encode.Builder
encodeList (NBTList t payload) =
  (Encode.word8 . toEnum . fromEnum $ t)
  <> (Encode.int32BE . toEnum $ length payload)
  <> foldl' (<>) mempty (fmap encodeNBT' payload)

encodeCompound :: [NBT] -> Encode.Builder
encodeCompound payload = foldr (<>) (Encode.word8 0x00) (fmap encodeNBT payload)

encodeIntArray :: (AU.UArray Int32 Int32) -> Encode.Builder
encodeIntArray arr =
  (Encode.int32BE . (\(a,b) -> toEnum . fromEnum $ AU.rangeSize (a,b)) $ (A.bounds arr))
  <> (foldl' (<>) mempty (fmap Encode.int32BE (AU.elems arr)))
