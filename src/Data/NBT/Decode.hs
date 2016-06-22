{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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

decodeNBT :: Decode.Parser NBT
decodeNBT = do
  tagTypeByte <- Decode.anyWord8
  case tagTypeByte of
    0x01 -> TagByte <$!> decodeText <*> decodeInt8
    0x02 -> TagShort <$!> decodeText <*> decodeInt16BE
    0x03 -> TagInt <$!> decodeText <*> decodeInt32BE
    0x04 -> TagLong <$!> decodeText <*> decodeInt64BE
    0x05 -> TagFloat <$!> decodeText <*> decodeFloatBE
    0x06 -> TagDouble <$!> decodeText <*> decodeDoubleBE
    0x07 -> TagByteArray <$!> decodeText <*> decodeByteArray
    0x08 -> TagString <$!> decodeText <*> decodeText
    0x09 -> TagList <$!> decodeText <*> decodeList
    0x0a -> TagCompound <$!> decodeText <*> decodeCompound
    0x0b -> TagIntArray <$!> decodeText <*> decodeIntArray

decodeNBT' :: TagType -> Decode.Parser NamelessNBT
decodeNBT' t = do
  case t of
    TypeByte -> NTagByte <$!> decodeInt8
    TypeShort -> NTagShort <$!> decodeInt16BE
    TypeInt -> NTagInt <$!> decodeInt32BE
    TypeLong -> NTagLong <$!> decodeInt64BE
    TypeFloat -> NTagFloat <$!> decodeFloatBE
    TypeDouble -> NTagDouble <$!> decodeDoubleBE
    TypeByteArray -> NTagByteArray <$!> decodeByteArray
    TypeString -> NTagString <$!> decodeText
    TypeList -> NTagList <$!> decodeList
    TypeCompound -> NTagCompound <$!> decodeCompound
    TypeIntArray -> NTagIntArray <$!> decodeIntArray

decodeWord16BE :: Decode.Parser Word16
decodeWord16BE = do
    bs <- Decode.take 2
    return $!
      (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 8) .|.
      (fromIntegral (bs `B.unsafeIndex` 1))
{-# INLINE decodeWord16BE #-}

decodeWord32BE :: Decode.Parser Word32
decodeWord32BE = do
    bs <- Decode.take 4
    return $!
      (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 24) .|.
      (fromIntegral (bs `B.unsafeIndex` 1) `shiftL` 16) .|.
      (fromIntegral (bs `B.unsafeIndex` 2) `shiftL` 8) .|.
      (fromIntegral (bs `B.unsafeIndex` 3))
{-# INLINE decodeWord32BE #-}

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
{-# INLINE decodeWord64BE #-}

decodeInt8 :: Decode.Parser Int8
decodeInt8  = do
  bs <- Decode.take 1
  return $! fromIntegral (B.unsafeHead bs)
{-# INLINE decodeInt8 #-}

decodeInt16BE :: Decode.Parser Int16
decodeInt16BE = do
    bs <- Decode.take 2
    return $! (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 8) .|.
              (fromIntegral (bs `B.unsafeIndex` 1))
{-# INLINE decodeInt16BE #-}

decodeInt32BE :: Decode.Parser Int32
decodeInt32BE = do
    bs <- Decode.take 4
    return $!
      (fromIntegral (bs `B.unsafeIndex` 0) `shiftL` 24) .|.
      (fromIntegral (bs `B.unsafeIndex` 1) `shiftL` 16) .|.
      (fromIntegral (bs `B.unsafeIndex` 2) `shiftL`  8) .|.
      (fromIntegral (bs `B.unsafeIndex` 3))
{-# INLINE decodeInt32BE #-}

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
{-# INLINE decodeInt64BE #-}

decodeFloatBE :: Decode.Parser Float
decodeFloatBE = wordToFloat <$> decodeWord32BE
{-# INLINE decodeFloatBE #-}

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)
{-# INLINE wordToFloat #-}

decodeDoubleBE :: Decode.Parser Double
decodeDoubleBE = wordToDouble <$> decodeWord64BE
{-# INLINE decodeDoubleBE #-}

wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)
{-# INLINE wordToDouble #-}

cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
{-# INLINE cast #-}

decodeByteArray :: Decode.Parser (UArray Int32 Int8)
decodeByteArray = do
  i <- decodeInt32BE
  lst <- Decode.count (fromEnum i) decodeInt8
  return $ array (0,(i-1)) (zip (range (0,(i-1))) lst)
{-# INLINE decodeByteArray #-}

decodeText :: Decode.Parser T.Text
decodeText = do
  ln <- fmap fromIntegral decodeInt16BE
  if ln /= 0
    then fmap decodeUtf8 (Decode.take ln)
    else return ""
{-# INLINE decodeText #-}

decodeList :: Decode.Parser NBTList
decodeList = do
  t <- Decode.anyWord8
  i <- decodeInt32BE
  lst <- Decode.count (fromEnum i) (decodeNBT' (toEnum . fromEnum $ t))
  return $ NBTList (toEnum . fromEnum $ t) lst
{-# INLINE decodeList #-}

decodeCompound :: Decode.Parser [NBT]
decodeCompound = Decode.manyTill' decodeNBT (Decode.word8 0x00)
{-# INLINE decodeCompound #-}

decodeIntArray :: Decode.Parser (UArray Int32 Int32)
decodeIntArray = do
  i <- decodeInt32BE
  lst <- Decode.count (fromEnum i) decodeInt32BE
  return $ array (0,(i-1)) (zip (range (0,(i-1))) lst)
{-# INLINE decodeIntArray #-}
