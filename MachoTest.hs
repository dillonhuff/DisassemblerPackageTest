module MachoTest(main) where

import Control.Monad
import Data.ByteString as BS
import Data.ByteString.Char8 as Ch
import Data.List as L
import Data.Macho
import Data.Word
import System.IO as IO
import Text.Disassembler.X86Disassembler

main :: IO ()
main = do
    handle <- openFile "testCProgram.o" ReadMode
    hSetBinaryMode handle True
    contents <- IO.hGetContents handle
    let byteStr = Ch.pack contents in
      do
        IO.putStrLn $ show byteStr ++ "\n\n\n"
        let machoFileStr = L.concatMap (\x -> (show x) ++ "\n\n") $ m_commands $ parseMacho byteStr in
          do
            IO.putStrLn machoFileStr
            hClose handle

strToWord8 :: String -> [Word8]
strToWord8 = BS.unpack . Ch.pack


