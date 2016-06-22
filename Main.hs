module Main where
import qualified  Data.Attoparsec.ByteString as Decode
import qualified  Data.ByteString as B
import qualified  Data.ByteString.Lazy as BL
import qualified  Codec.Compression.GZip as GZip

import Data.NBT.Decode

main :: IO ()
main = do
  tinyDat <- B.readFile "bench/benchdata/hello_world.nbt"
  rawBig <- B.readFile "bench/benchdata/bigtest.nbt"
  rawLevel <- B.readFile "bench/benchdata/level.dat"
  let bigDat = BL.toStrict $ GZip.decompress (BL.fromStrict rawBig)
  let levelDat = BL.toStrict $ GZip.decompress (BL.fromStrict rawLevel)
  case Decode.parseOnly decodeNBT tinyDat of
    Right _ -> return ()
    Left err -> print err
  case Decode.parseOnly decodeNBT bigDat of
    Right _ -> return ()
    Left err -> print err
  case Decode.parseOnly decodeNBT levelDat of
    Right _ -> return ()
    Left err -> print err
