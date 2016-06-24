{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import qualified  Data.Array.IArray as A
import qualified  Data.Array.Unboxed as AU
import qualified  Data.Attoparsec.ByteString as Decode
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.ByteString.Builder as Encode
import            Data.Either
import            Data.Int
import            Data.NBT.Decode
import            Data.NBT.Encode
import            Data.NBT.Types
import qualified  Data.Text as T
import            Data.Word
import qualified  Codec.Compression.GZip as GZip
import            Test.Hspec
import            Test.QuickCheck

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>),(<*>))
#endif

instance Arbitrary T.Text where
  arbitrary = fmap T.pack (arbitrary :: Gen String)

instance Arbitrary (A.Array Int32 Int8) where
  arbitrary = do
    ln <- choose (0,10) :: Gen Int32
    e <- vectorOf (fromEnum ln) arbitrary
    return $ A.array (0,(ln-1)) (zip (A.range (0,(ln-1))) e)

instance Arbitrary (AU.UArray Int32 Int32) where
  arbitrary = do
    ln <- choose (0,10) :: Gen Int32
    e <- vectorOf (fromEnum ln) arbitrary
    return $ AU.array (0,(ln-1)) (zip (AU.range (0,(ln-1))) e)

instance Arbitrary (AU.UArray Int32 Int8) where
  arbitrary = do
    ln <- choose (0,10) :: Gen Int32
    e <- vectorOf (fromEnum ln) arbitrary
    return $ AU.array (0,(ln-1)) (zip (AU.range (0,(ln-1))) e)

instance Arbitrary NBT where
  arbitrary = sized nbt'
    where nbt' 0 = return $ TagByte "" 5
          nbt' n | n > 0 = do
            tagID <- choose (0x01,0x0b) :: Gen Word8

            case tagID of
              0x01 -> TagByte <$> arbitrary <*> arbitrary
              0x02 -> TagShort <$> arbitrary <*> arbitrary
              0x03 -> TagInt <$> arbitrary <*> arbitrary
              0x04 -> TagLong <$> arbitrary <*> arbitrary
              0x05 -> TagFloat <$> arbitrary <*> arbitrary
              0x06 -> TagDouble <$> arbitrary <*> arbitrary
              0x07 -> TagByteArray <$> arbitrary <*> arbitrary
              0x08 -> TagString <$> arbitrary <*> arbitrary
              0x09 -> TagList <$> arbitrary <*> arbitrary
              0x0a -> TagCompound <$> arbitrary <*> vectorOf (n `div` 30) arbitrary
              0x0b -> TagIntArray <$> arbitrary <*> arbitrary

instance Arbitrary NBTList where
  arbitrary = sized nbtlst'
    where nbtlst' 0 = return $ NBTList TypeByte [NTagByte 0]
          nbtlst' n | n > 0 = do
            tagID <- choose (0x01,0x0b) :: Gen Word8
            i <- choose (0,5) :: Gen Int
            case tagID of
              0x01 -> NBTList <$> return TypeByte <*> vectorOf i (NTagByte <$> (arbitrary :: Gen Int8))
              0x02 -> NBTList <$> return TypeShort <*> vectorOf i (NTagShort <$> (arbitrary :: Gen Int16))
              0x03 -> NBTList <$> return TypeInt <*> vectorOf i (NTagInt <$> (arbitrary :: Gen Int32))
              0x04 -> NBTList <$> return TypeLong <*> vectorOf i (NTagLong <$> (arbitrary :: Gen Int64))
              0x05 -> NBTList <$> return TypeFloat <*> vectorOf i (NTagFloat <$> (arbitrary :: Gen Float))
              0x06 -> NBTList <$> return TypeDouble <*> vectorOf i (NTagDouble <$> (arbitrary :: Gen Double))
              0x07 -> NBTList <$> return TypeByteArray <*> vectorOf (n `div` 20) (NTagByteArray <$> (arbitrary :: Gen (AU.UArray Int32 Int8)))
              0x08 -> NBTList <$> return TypeString <*> vectorOf i (NTagString <$> (arbitrary :: Gen T.Text))
              0x09 -> NBTList <$> return TypeList <*> vectorOf (n `div` 20) (NTagList <$> arbitrary)
              0x0a -> NBTList <$> return TypeCompound <*> vectorOf (n `div` 30) (NTagCompound <$> vectorOf (n `div` 30) (arbitrary :: Gen NBT))
              0x0b -> NBTList <$> return TypeIntArray <*> vectorOf (n `div` 20) (NTagIntArray <$> (arbitrary :: Gen (AU.UArray Int32 Int32)))

checkIdentity :: Eq a => [a] -> (a -> Encode.Builder) -> (Decode.Parser a) -> Bool
checkIdentity [] _ _ = True
checkIdentity lst f f' = do
  let encoded = fmap (\x -> BL.toStrict . Encode.toLazyByteString $ (f x)) lst
  let decoded = fmap (Decode.parseOnly f') encoded
  lst == (rights decoded)

prop_ByteIdentity :: [Int8] -> Bool
prop_ByteIdentity lst = checkIdentity lst Encode.int8 decodeInt8

prop_ShortIdentity :: [Int16] -> Bool
prop_ShortIdentity lst = checkIdentity lst Encode.int16BE decodeInt16BE

prop_IntIdentity :: [Int32] -> Bool
prop_IntIdentity lst = checkIdentity lst Encode.int32BE decodeInt32BE

prop_LongIdentity :: [Int64] -> Bool
prop_LongIdentity lst = checkIdentity lst Encode.int64BE decodeInt64BE

prop_FloatIdentity :: [Float] -> Bool
prop_FloatIdentity lst = checkIdentity lst Encode.floatBE decodeFloatBE

prop_DoubleIdentity :: [Double] -> Bool
prop_DoubleIdentity lst = checkIdentity lst Encode.doubleBE decodeDoubleBE

prop_ByteArrayIdentity :: [(AU.UArray Int32 Int8)] -> Bool
prop_ByteArrayIdentity lst = checkIdentity lst encodeByteArray decodeByteArray

prop_StringIdentity :: [T.Text] -> Bool
prop_StringIdentity lst = checkIdentity lst encodeText decodeText

prop_ListIdentity :: [NBTList] -> Bool
prop_ListIdentity lst = checkIdentity lst encodeList decodeList

prop_CompoundIdentity :: [[NBT]] -> Bool
prop_CompoundIdentity lst = checkIdentity lst encodeCompound decodeCompound

prop_IntArrayIdentity :: [(AU.UArray Int32 Int32)] -> Bool
prop_IntArrayIdentity lst = checkIdentity lst encodeIntArray decodeIntArray

main :: IO ()
main = hspec $ do
  describe "Payload encoders/decoders" $ do
    context "Byte" $ do
      it "Identity" $ property prop_ByteIdentity
    context "Short" $ do
      it "Identity" $ property prop_ShortIdentity
    context "Int" $ do
      it "Identity" $ property prop_IntIdentity
    context "Long" $ do
      it "Identity" $ property prop_LongIdentity
    context "Float" $ do
      it "Identity" $ property prop_FloatIdentity
    context "Double" $ do
      it "Identity" $ property prop_DoubleIdentity
    context "ByteArray" $ do
      it "Identity" $ property prop_ByteArrayIdentity
    context "String" $ do
      it "Identity" $ property prop_StringIdentity
    context "List" $ do
      it "Identity" $ property prop_ListIdentity
    context "Compound" $ do
      it "Identity" $ property prop_CompoundIdentity
    context "IntArray" $ do
      it "Identity" $ property prop_IntArrayIdentity
  describe "Decoding NBT formatted files" $ do
    context "hello_world.nbt" $ do
      it "Should return fully parsed Right" $ do
        let possiblyDecoded =
              fmap
                (Decode.parseOnly decodeNBT)
                (B.readFile "test/testdata/hello_world.nbt")
        possiblyDecoded >>= (`shouldSatisfy` isRight)
    context "level.dat" $ do
      it "Should return fully parsed Right" $ do
        let possiblyDecoded =
              fmap
                ((Decode.parseOnly decodeNBT)
                  . BL.toStrict
                  . GZip.decompress
                  . BL.fromStrict
                )
                (B.readFile "test/testdata/level.dat")
        possiblyDecoded >>= (`shouldSatisfy` isRight)
    context "bigtest.nbt" $ do
      it "Should return fully parsed Right" $ do
        let possiblyDecoded =
              fmap
                ((Decode.parseOnly decodeNBT)
                  . BL.toStrict
                  . GZip.decompress
                  . BL.fromStrict
                )
                (B.readFile "test/testdata/bigtest.nbt")
        possiblyDecoded >>= (`shouldSatisfy` isRight)
