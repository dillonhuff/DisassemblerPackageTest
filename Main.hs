module Main(main) where

import Control.Monad
import Data.ByteString as BS
import Data.ByteString.Char8 as Ch
import Data.List as L
import Data.Word
import System.IO as IO
import Text.Disassembler.X86Disassembler

main :: IO ()
main = do
    handle <- openFile "testCProgram.o" ReadMode
    hSetBinaryMode handle True
    contents <- IO.hGetContents handle
    IO.putStrLn contents
    let contentsBytes = strToWord8 contents in
      do
        IO.putStrLn $ show contentsBytes
        IO.putStrLn $ "Length of file is " ++ show (Prelude.length contentsBytes) ++ " bytes"
--        disassembledInstrs <- mapM disassembleList $ L.inits contentsBytes
--        IO.putStrLn $ show disassembledInstrs
        hClose handle

strToWord8 :: String -> [Word8]
strToWord8 = BS.unpack . Ch.pack

