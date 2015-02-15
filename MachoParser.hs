module MachoParser() where

import Control.Monad
import Data.ByteString as BS
import Data.Macho

pureInstructionSequences :: (Monad m) => ByteString -> m [(Either ParseError [Instruction])]
pureInstructionSequences byteStr = mapM disassembleList instrByteSeqs
  where
    instrByteSeqs = map BS.unpack $ pureInstrBytes machoFile byteStr
    machoFile = parseMacho byteStr

pureIntrBytes :: Macho -> ByteString -> [ByteString]
pureInstrBytes 
