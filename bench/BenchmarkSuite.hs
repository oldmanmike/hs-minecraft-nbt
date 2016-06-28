import Criterion.Main
import qualified  Data.Attoparsec.ByteString as Decode
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Data.ByteString.Builder as Encode
import qualified  Codec.Compression.GZip as GZip

import Data.NBT.Decode

main :: IO ()
main = do
  tinyDat <- B.readFile "bench/benchdata/hello_world.nbt"
  rawBig <- B.readFile "bench/benchdata/bigtest.nbt"
  rawLevel <- B.readFile "bench/benchdata/level.dat"
  let bigDat = BL.toStrict $ GZip.decompress (BL.fromStrict rawBig)
  let levelDat = BL.toStrict $ GZip.decompress (BL.fromStrict rawLevel)
  defaultMain
    [ bgroup "decodeNBT"
      [ bench "tiny" $ nf (Decode.parseOnly decodeNBT) tinyDat
      , bench "big" $ nf (Decode.parseOnly decodeNBT) bigDat
      , bench "level" $ nf (Decode.parseOnly decodeNBT) levelDat
      ]
    , bgroup "decodeInt8"
      [ bench "100" $ nf (Decode.parseOnly decodeInt8) (B.replicate 100 0)
      , bench "1000" $ nf (Decode.parseOnly decodeInt8) (B.replicate 1000 0)
      , bench "10000" $ nf (Decode.parseOnly decodeInt8) (B.replicate 10000 0)
      ]
    , bgroup "decodeInt16BE"
      [ bench "100" $ nf (Decode.parseOnly decodeInt16BE) (B.replicate (100 * 2) 0)
      , bench "1000" $ nf (Decode.parseOnly decodeInt16BE) (B.replicate (1000 * 2) 0)
      , bench "10000" $ nf (Decode.parseOnly decodeInt16BE) (B.replicate (10000 * 2) 0)
      ]
    , bgroup "decodeInt32BE"
      [ bench "100" $ nf (Decode.parseOnly decodeInt32BE) (B.replicate (100 * 4) 0)
      , bench "1000" $ nf (Decode.parseOnly decodeInt32BE) (B.replicate (1000 * 4) 0)
      , bench "10000" $ nf (Decode.parseOnly decodeInt32BE) (B.replicate (10000 * 4) 0)
      ]
    , bgroup "decodeInt64BE"
      [ bench "100" $ nf (Decode.parseOnly decodeInt64BE) (B.replicate (100 * 8) 0)
      , bench "1000" $ nf (Decode.parseOnly decodeInt64BE) (B.replicate (1000 * 8) 0)
      , bench "10000" $ nf (Decode.parseOnly decodeInt64BE) (B.replicate (10000 * 8) 0)
      ]
    , bgroup "decodeWord16BE"
      [ bench "100" $ nf (Decode.parseOnly decodeWord16BE) (B.replicate (100 * 2) 0)
      , bench "1000" $ nf (Decode.parseOnly decodeWord16BE) (B.replicate (1000 * 2) 0)
      , bench "10000" $ nf (Decode.parseOnly decodeWord16BE) (B.replicate (10000 * 2) 0)
      ]
    , bgroup "decodeWord32BE"
      [ bench "100" $ nf (Decode.parseOnly decodeWord32BE) (B.replicate (100 * 4) 0)
      , bench "1000" $ nf (Decode.parseOnly decodeWord32BE) (B.replicate (1000 * 4) 0)
      , bench "10000" $ nf (Decode.parseOnly decodeWord32BE) (B.replicate (10000 * 4) 0)
      ]
    , bgroup "decodeWord64BE"
      [ bench "100" $ nf (Decode.parseOnly decodeWord64BE) (B.replicate (100 * 8) 0)
      , bench "1000" $ nf (Decode.parseOnly decodeWord64BE) (B.replicate (1000 * 8) 0)
      , bench "10000" $ nf (Decode.parseOnly decodeWord64BE) (B.replicate (10000 * 8) 0)
      ]
      {-
    , bgroup "decodeFloatBE" []
      [ bench "100" $ whnf (Decode.parseOnly decodeFloatBE) (B.replicateM Encode.int8
      , bench "1000" $ whnf (Decode.parseOnly decodeFloatBE) (B.replicateM Encode.int8
      , bench "10000" $ whnf (Decode.parseOnly decodeFloatBE) (B.replicateM Encode.int8
      ]
    , bgroup "decodeDoubleBE" []
      [ bench "100" $ whnf (Decode.parseOnly decodeDoubleBE) (B.replicateM Encode.int8
      , bench "1000" $ whnf (Decode.parseOnly decodeDoubleBE) (B.replicateM Encode.int8
      , bench "10000" $ whnf (Decode.parseOnly decodeDoubleBE) (B.replicateM Encode.int8
      ]
    , bgroup "decodeText" []
      [ bench "100" $ whnf (Decode.parseOnly decodeText) (B.replicateM Encode.int8
      , bench "1000" $ whnf (Decode.parseOnly decodeText) (B.replicateM Encode.int8
      , bench "10000" $ whnf (Decode.parseOnly decodeText) (B.replicateM Encode.int8
      ]
    , bgroup "decodeByteArray" []
      [ bench "100" $ whnf (Decode.parseOnly decodeByteArray) (B.replicateM Encode.int8
      , bench "1000" $ whnf (Decode.parseOnly decodeByteArray) (B.replicateM Encode.int8
      , bench "10000" $ whnf (Decode.parseOnly decodeByteArray) (B.replicateM Encode.int8
      ]
    , bgroup "decodeList" []
      [ bench "100" $ whnf (Decode.parseOnly decodeList) (B.replicateM Encode.int8
      , bench "1000" $ whnf (Decode.parseOnly decodeList) (B.replicateM Encode.int8
      , bench "10000" $ whnf (Decode.parseOnly decodeList) (B.replicateM Encode.int8
      ]
    , bgroup "decodeCompound" []
      [ bench "100" $ whnf (Decode.parseOnly decodeCompound) (B.replicateM Encode.int8
      , bench "1000" $ whnf (Decode.parseOnly decodeCompound) (B.replicateM Encode.int8
      , bench "10000" $ whnf (Decode.parseOnly decodeCompound) (B.replicateM Encode.int8
      ]
    , bgroup "decodeIntArray" []
      [ bench "100" $ whnf (Decode.parseOnly decodeIntArray) (B.replicateM Encode.int8
      , bench "1000" $ whnf (Decode.parseOnly decodeIntArray) (B.replicateM Encode.int8
      , bench "10000" $ whnf (Decode.parseOnly decodeIntArray) (B.replicateM Encode.int8
      ]
      -}
    ]
