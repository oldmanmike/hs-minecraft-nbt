module Data.NBT.Encode
  ( encodeNBT
  ) where

import qualified  Data.Array.IArray as A
import qualified  Data.Array.Unboxed as AU
import qualified  Data.ByteString.Builder as Encode
import            Data.Int
import            Data.List
import            Data.Monoid
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.Word

import            Data.NBT.Types (NBT(..))

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
  <> (Encode.int32BE $ (\(a,b) -> toEnum . fromEnum $ b - a) $ (AU.bounds payload))
  <> (foldl' (<>) mempty (fmap Encode.int8 (AU.elems payload)))

encodeNBT (TagString name payload) =
  Encode.word8 0x08
  <> encodeText name
  <> encodeText payload

encodeNBT (TagList name payload) =
  Encode.word8 0x09
  <> encodeText name
  <> (Encode.int32BE . (\(a,b) -> toEnum . fromEnum $ b - a) $ (A.bounds payload))
  <> foldl' (<>) mempty (fmap Encode.int8 payload)

encodeNBT (TagCompound name payload) =
  Encode.word8 0x0a
  <> encodeText name
  <> (foldl' (<>) mempty (fmap encodeNBT payload))

encodeNBT (TagIntArray name payload) =
  Encode.word8 0x0b
  <> encodeText name
  <> (Encode.int32BE . (\(a,b) -> toEnum . fromEnum $ b - a) $ (A.bounds payload))
  <> (foldl' (<>) mempty (fmap Encode.int32BE (AU.elems payload)))

encodeText :: T.Text -> Encode.Builder
encodeText t =
  (Encode.int16BE . toEnum . T.length $ t)
  <> encodeUtf8Builder t
