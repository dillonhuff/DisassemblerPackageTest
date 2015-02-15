module MachoParser() where

import Control.Monad
import Data.ByteString as BS
import Data.List as L
import Data.Macho
import Data.Word
import Text.Disassembler.X86Disassembler
import Text.Parsec.Error

sanityCheckSegmentSize :: MachoSegment -> Bool
sanityCheckSegmentSize segment = segmentSize == sumOfSectionSizes
  where
    segmentSize = seg_filesize segment
    sumOfSectionSizes = L.foldl (+)  0 $ L.map sec_size $ seg_sections segment

pureInstructionSequences :: (Monad m) => ByteString -> m [(Either ParseError [Instruction])]
pureInstructionSequences byteStr = mapM disassembleList instrByteSeqs
  where
    instrByteSeqs = L.map BS.unpack $ pureInstrBytes machoFile byteStr
    machoFile = parseMacho byteStr

pureInstrBytes :: Macho -> ByteString -> [ByteString]
pureInstrBytes macho fileBytes = L.map (extractBytes fileBytes) instrSeqLocations
  where
    instrSeqLocations = pureInstructionSequenceLocations macho

data ByteRange = ByteRange {
     offset :: Word64,
     size :: Word64
  } deriving (Eq, Ord)

byteRange = ByteRange

extractBytes :: ByteString -> ByteRange -> ByteString
extractBytes byteStr range =
  BS.take (fromIntegral $ size range) $ BS.drop (fromIntegral $ offset range) byteStr

pureInstructionSequenceLocations :: Macho -> [ByteRange]
pureInstructionSequenceLocations macho = pureInstrLocs
  where
    commands = m_commands macho
    sec64s = L.concatMap extractSeg64 commands
    pureInstrLocs = []

extractSeg64 :: LC_COMMAND -> [MachoSegment]
extractSeg64 (LC_SEGMENT_64 seg) = [seg]
extractSeg64 _ = []
