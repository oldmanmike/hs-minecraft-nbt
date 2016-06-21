{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
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
import            Debug.Trace
import            Test.Hspec
import            Test.QuickCheck

instance Arbitrary T.Text where
  arbitrary = fmap T.pack (arbitrary :: Gen String)

instance Arbitrary (A.Array Int32 Int8) where
  arbitrary = do
    ln <- choose (0,1000) :: Gen Int32
    e <- vectorOf (fromEnum ln) arbitrary
    return $ A.array (0,(ln-1)) (zip (A.range (0,(ln-1))) e)

instance Arbitrary (AU.UArray Int32 Int32) where
  arbitrary = do
    ln <- choose (0,1000) :: Gen Int32
    e <- vectorOf (fromEnum ln) arbitrary
    return $ AU.array (0,(ln-1)) (zip (AU.range (0,(ln-1))) e)

instance Arbitrary (AU.UArray Int32 Int8) where
  arbitrary = do
    ln <- choose (0,1000) :: Gen Int32
    e <- vectorOf (fromEnum ln) arbitrary
    return $ AU.array (0,(ln-1)) (zip (AU.range (0,(ln-1))) e)

instance Arbitrary NBT where
  arbitrary = do
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
      0x0a -> TagCompound <$> arbitrary <*> arbitrary
      0x0b -> TagIntArray <$> arbitrary <*> arbitrary

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

prop_ListIdentity :: [(A.Array Int32 Int8)] -> Bool
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
