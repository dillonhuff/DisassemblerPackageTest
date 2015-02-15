module BinaryIO(binaryFileContents) where

import Data.ByteString.Char8 as Ch
import System.IO as IO

binaryFileContents :: FilePath -> IO ByteString
binaryFileContents path = do
  handle <- openFile path ReadMode
  hSetBinaryMode handle True
  contents <- IO.hGetContents handle
  return $ Ch.pack contents
