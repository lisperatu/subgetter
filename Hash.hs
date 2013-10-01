module Subgetter.Hash (shortsum, showSum) where
import IO(bracket)
import System.Environment(getArgs)
import System.IO(openBinaryFile,hClose,hFileSize,hSeek,IOMode(ReadMode),SeekMode(AbsoluteSeek,SeekFromEnd))
import qualified Data.ByteString.Lazy as L(hGet,unpack)
import Data.Binary.Get(runGet,getWord64le)
import Data.Binary.Put(runPut,putWord64le)
import Data.Word(Word64)
import Control.Monad(foldM)
import Data.Bits.Utils(w82s)
import Data.Hex(hex)

shortsum :: FilePath -> IO Word64
shortsum filename = bracket (openBinaryFile filename ReadMode) hClose $ \h -> do
  fs <- hFileSize h
  hSeek h AbsoluteSeek 0 ; begin <- L.hGet h chunksize
  hSeek h SeekFromEnd (-(toInteger chunksize)) ; end <- L.hGet h chunksize
  return $ (flip runGet $ begin) $ chunksum $ (flip runGet $ end) (chunksum . fromInteger $ fs)
  where
    chunksize = 0x10000
    chunksum n = foldM (\a _ -> getWord64le >>= return . (+a)) n [1..(chunksize`div`8)]

main :: IO ()
main = do
  args <- getArgs
  let fn = head $ args
  p <- shortsum fn
  putStrLn $ "The hash of file " ++ fn ++ ": " ++ (hex $ w82s $ reverse (L.unpack $ runPut $ putWord64le p))

showSum sum = hex $ w82s $ reverse (L.unpack $ runPut $ putWord64le sum)
