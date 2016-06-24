{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
-------------------------------------------------------------------------------
-- |
-- Module       : Data.NBT.Decode
-- Copyright    : (c) 2016 Michael Carpenter
-- License      : BSD3
-- Maintainer   : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability    : experimental
-- Portability  : non-portable
--
-------------------------------------------------------------------------------
module Data.NBT.Decode
  ( decodeNBT
  , decodeInt8
  , decodeInt16BE
  , decodeInt32BE
  , decodeInt64BE
  , decodeFloatBE
  , decodeDoubleBE
  , decodeByteArray
  , decodeText
  , decodeList
  , decodeCompound
  , decodeIntArray
  ) where

import            Control.Monad
import            Control.Monad.ST
import            Data.Array.ST (newArray, readArray, MArray, STUArray)
import            Data.Array.Unboxed
import            Data.Array.Unsafe (castSTUArray)
import qualified  Data.Attoparsec.ByteString as Decode
import            Data.Bits
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Unsafe as B
import            Data.Int
import qualified  Data.Text as T
import            Data.Text.Encoding
import            Data.Word
import            Data.NBT.Types

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>),(<*>))
#endif

decodeNBT :: Decode.Parser NBT
decodeNBT = do
  tagTypeByte <- Decode.anyWord8
  case tagTypeByte of
    0x01 -> TagByte <$> decodeText <*> decodeInt8
    0x02 -> TagShort <$> decodeText <*> decodeInt16BE
    0x03 -> TagInt <$> decodeText <*> decodeInt32BE
    0x04 -> TagLong <$> decodeText <*> decodeInt64BE
    0x05 -> TagFloat <$> decodeText <*> decodeFloatBE
    0x06 -> TagDouble <$> decodeText <*> decodeDoubleBE
    0x07 -> TagByteArray <$> decodeText <*> decodeByteArray
    0x08 -> TagString <$> decodeText <*> decodeText
    0x09 -> TagList <$> decodeText <*> decodeList
    0x0a -> TagCompound <$> decodeText <*> decodeCompound
    0x0b -> TagIntArray <$> decodeText <*> decodeIntArray
    0x00 -> fail ("Could not match tag type: " ++ show tagTypeByte)

decodeNBT' :: TagType -> Decode.Parser NamelessNBT
decodeNBT' TypeByte       = NTagByte      <$> decodeInt8
decodeNBT' TypeShort      = NTagShort     <$> decodeInt16BE
decodeNBT' TypeInt        = NTagInt       <$> decodeInt32BE
decodeNBT' TypeLong       = NTagLong      <$> decodeInt64BE
decodeNBT' TypeFloat      = NTagFloat     <$> decodeFloatBE
decodeNBT' TypeDouble     = NTagDouble    <$> decodeDoubleBE
decodeNBT' TypeByteArray  = NTagByteArray <$> decodeByteArray
decodeNBT' TypeString     = NTagString    <$> decodeText
decodeNBT' TypeList       = NTagList      <$> decodeList
decodeNBT' TypeCompound   = NTagCompound  <$> decodeCompound
decodeNBT' TypeIntArray   = NTagIntArray  <$> decodeIntArray

decodeWord16BE :: Decode.Parser Word16
decodeWord16BE = do
    bs <- Decode.take 2
    return $!
      (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 8) .|.
      (fromIntegral (bs `B.unsafeIndex` 1))

decodeWord32BE :: Decode.Parser Word32
decodeWord32BE = do
    bs <- Decode.take 4
    return $!
      (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 24) .|.
      (fromIntegral (bs `B.unsafeIndex` 1) `shiftL` 16) .|.
      (fromIntegral (bs `B.unsafeIndex` 2) `shiftL` 8) .|.
      (fromIntegral (bs `B.unsafeIndex` 3))

decodeWord64BE :: Decode.Parser Word64
decodeWord64BE = do
    bs <- Decode.take 8
    return $!
      (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 56) .|.
      (fromIntegral (bs `B.unsafeIndex` 1) `shiftL` 48) .|.
      (fromIntegral (bs `B.unsafeIndex` 2) `shiftL` 40) .|.
      (fromIntegral (bs `B.unsafeIndex` 3) `shiftL` 32) .|.
      (fromIntegral (bs `B.unsafeIndex` 4) `shiftL` 24) .|.
      (fromIntegral (bs `B.unsafeIndex` 5) `shiftL` 16) .|.
      (fromIntegral (bs `B.unsafeIndex` 6) `shiftL`  8) .|.
      (fromIntegral (bs `B.unsafeIndex` 7))

decodeInt8 :: Decode.Parser Int8
decodeInt8  = do
  bs <- Decode.take 1
  return $! fromIntegral (B.unsafeHead bs)

decodeInt16BE :: Decode.Parser Int16
decodeInt16BE = do
    bs <- Decode.take 2
    return $! (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 8) .|.
              (fromIntegral (bs `B.unsafeIndex` 1))

decodeInt32BE :: Decode.Parser Int32
decodeInt32BE = do
    bs <- Decode.take 4
    return $!
      (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 24) .|.
      (fromIntegral (bs `B.unsafeIndex` 1) `shiftL` 16) .|.
      (fromIntegral (bs `B.unsafeIndex` 2) `shiftL`  8) .|.
      (fromIntegral (bs `B.unsafeIndex` 3))

decodeInt64BE :: Decode.Parser Int64
decodeInt64BE = do
    bs <- Decode.take 8
    return $!
      (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 56) .|.
      (fromIntegral (bs `B.unsafeIndex` 1) `shiftL` 48) .|.
      (fromIntegral (bs `B.unsafeIndex` 2) `shiftL` 40) .|.
      (fromIntegral (bs `B.unsafeIndex` 3) `shiftL` 32) .|.
      (fromIntegral (bs `B.unsafeIndex` 4) `shiftL` 24) .|.
      (fromIntegral (bs `B.unsafeIndex` 5) `shiftL` 16) .|.
      (fromIntegral (bs `B.unsafeIndex` 6) `shiftL`  8) .|.
      (fromIntegral (bs `B.unsafeIndex` 7))

decodeFloatBE :: Decode.Parser Float
decodeFloatBE = wordToFloat <$> decodeWord32BE

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

decodeDoubleBE :: Decode.Parser Double
decodeDoubleBE = wordToDouble <$> decodeWord64BE

wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

decodeByteArray :: Decode.Parser (UArray Int32 Int8)
decodeByteArray = do
  i <- decodeInt32BE
  lst <- Decode.count (fromEnum i) decodeInt8
  return $ array (0,(i-1)) (zip (range (0,(i-1))) lst)

decodeText :: Decode.Parser T.Text
decodeText = do
  ln <- fmap fromIntegral decodeInt16BE
  fmap decodeUtf8 (Decode.take ln)

decodeList :: Decode.Parser NBTList
decodeList = do
  t <- Decode.anyWord8
  i <- decodeInt32BE
  lst <- Decode.count (fromEnum i) (decodeNBT' (toEnum . fromEnum $ t))
  return $ NBTList (toEnum . fromEnum $ t) lst

decodeCompound :: Decode.Parser [NBT]
decodeCompound = Decode.manyTill' decodeNBT (Decode.word8 0x00)

decodeIntArray :: Decode.Parser (UArray Int32 Int32)
decodeIntArray = do
  i <- decodeInt32BE
  lst <- Decode.count (fromEnum i) decodeInt32BE
  return $ array (0,(i-1)) (zip (range (0,(i-1))) lst)
