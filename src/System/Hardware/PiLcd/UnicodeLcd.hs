{-|
Module      : System.Hardware.PiLcd.UnicodeLcd
Description : Display Unicode characters on an LCD
Copyright   : © Patrick Pelletier, 2017
License     : BSD3
Maintainer  : code@funwithsoftware.org

Displays Unicode text on an LCD.  Only updates the parts
of the LCD which have changed.  Automatically manages
custom characters, using the
<https://www.cl.cam.ac.uk/~mgk25/ucs-fonts.html 5x8 fixed font>
for characters which are not built-in to the the LCD controller's ROM.
Only eight distinct non-built-in characters can be on the display
at any one time.

Only supports characters which are made up of a single code point.
(In other words, combining marks are not supported.)  If your input
contains decomposed characters, consider using the
<https://hackage.haskell.org/package/unicode-transforms unicode-transforms>
package to convert to Normalization Form C.
-}

module System.Hardware.PiLcd.UnicodeLcd
  ( Lcd
  , LcdOptions(..)
  , RomCode(..)
  , defaultLcdOptions
  -- , supportedChars
  , getCharStatus
  , CharStatus(..)
  , mkLcd
  , lcdOptions
  , updateDisplay
  ) where

import Control.Arrow
import Control.Monad
import Data.Char
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Text as T
import Data.Word

import System.Hardware.PiLcd.Font5x8
import System.Hardware.PiLcd.Hd44780
import System.Hardware.PiLcd.Util

-- | An opaque type representing an LCD.
data Lcd =
  Lcd
  { lcdOptions  :: LcdOptions  -- ^ Returns the 'LcdOptions' that were passed to 'mkLcd'
  , lcdCb       :: LcdCallbacks
  , lcdLines    :: IORef [B.ByteString]
  , lcdCustom   :: IORef CustomInfo
  , lcdEncoding :: CharEncoding
  }

type CustomInfo = (Integer, [(Char, Integer)])

data CharEncoding =
  CharEncoding
  { ceBuiltIn       :: EncodingHash
  , ceCustom        :: [(Char, [Word8])]
  , ceCustomMapping :: [Char]
  }

-- | Specifies the characteristics of the LCD.  Displays up to
-- 20x4 should be supported, although only 16x2 has been tested.
data LcdOptions =
  LcdOptions
  { loLines :: Int        -- ^ Number of lines
  , loColumns :: Int      -- ^ Number of columns
  , loRomCode :: RomCode  -- ^ Built-in character set
  , loCustomChars :: [(Char, [Word8])] -- ^ Additional user-defined characters,
                                       -- beyond those in the 5x8 fixed font.
                                       -- Character is defined as 8 bytes, with
                                       -- data in least-significant 5 bits.
  } deriving (Eq, Ord, Show, Read)

-- | Defaults to 2 lines, 16 columns, ROM code A00, and no additional
-- custom characters.
defaultLcdOptions :: LcdOptions
defaultLcdOptions =
  LcdOptions
  { loLines = 2
  , loColumns = 16
  , loRomCode = RomA00
  , loCustomChars = []
  }

-- The HD44780U LCD controller comes in two different variants with
-- different character ROMs.  (See Table 4 on pages 17-18 of the
-- <https://www.adafruit.com/datasheets/HD44780.pdf HD44780U datasheet>.)
-- Unfortunately, as best as I can interpret
-- <https://forums.adafruit.com/viewtopic.php?f=50&t=111019 this exchange with Adafruit customer support>,
-- Adafruit ships a mixture of A00 ROMs and A02 ROMs, depending on what's
-- available at the moment.
-- (\"We take what's available or we don't sell LCDs.\")  This is a bit
-- annoying, since there doesn't seem to be any way to query the HD44780U
-- to find out which ROM it has.  So, the user has to test their LCD
-- and then specify which ROM they have.

data RomCode = RomA00 | RomA02
             deriving (Eq, Ord, Show, Read, Bounded, Enum)

type EncodingHash = H.HashMap Char Word8

-- This list is incomplete; pull requests are welcome
tableA00 :: [(Int, Word8)]
tableA00 =
  [ (0x00A5, 0x5c)  -- ¥ YEN SIGN
  , (0x2192, 0x7e)  -- → RIGHTWARDS ARROW
  , (0x2190, 0x7f)  -- ← LEFTWARDS ARROW
  , (0x2219, 0xa5)  -- ∙ BULLET OPERATOR
  , (0x2203, 0xd6)  -- ∃ THERE EXISTS
  , (0x25AF, 0xdb)  -- ▯ WHITE VERTICAL RECTANGLE
  , (0x00B0, 0xdf)  -- ° DEGREE SIGN
  , (0x03B1, 0xe0)  -- α GREEK SMALL LETTER ALPHA
  , (0x00E4, 0xe1)  -- ä LATIN SMALL LETTER A WITH DIAERESIS
  , (0x03B2, 0xe2)  -- β GREEK SMALL LETTER BETA
  , (0x03B5, 0xe3)  -- ε GREEK SMALL LETTER EPSILON
  , (0x03BC, 0xe4)  -- μ GREEK SMALL LETTER MU
  , (0x03C3, 0xe5)  -- σ GREEK SMALL LETTER SIGMA
  , (0x03C1, 0xe6)  -- ρ GREEK SMALL LETTER RHO
  , (0x221A, 0xe8)  -- √ SQUARE ROOT
  , (0x00A2, 0xec)  -- ¢ CENT SIGN
  , (0x00F6, 0xef)  -- ö LATIN SMALL LETTER O WITH DIAERESIS
  , (0x0398, 0xf2)  -- Θ GREEK CAPITAL LETTER THETA
  , (0x03A9, 0xf4)  -- Ω GREEK CAPITAL LETTER OMEGA
  , (0x00FC, 0xf5)  -- ü LATIN SMALL LETTER U WITH DIAERESIS
  , (0x03A3, 0xf6)  -- Σ GREEK CAPITAL LETTER SIGMA
  , (0x03C0, 0xf7)  -- π GREEK SMALL LETTER PI
  , (0x00F7, 0xfd)  -- ÷ DIVISION SIGN
  , (0x2588, 0xff)  -- █ FULL BLOCK
  ]

hashA00 :: EncodingHash
hashA00 = mkTable tableA00 $ [0x20..0x5b] ++ [0x5d..0x7d]

tableA02 :: [(Int, Word8)]
tableA02 =
  [ (0x25B6, 0x10)  -- ▶ BLACK RIGHT-POINTING TRIANGLE
  , (0x25C0, 0x11)  -- ◀ BLACK LEFT-POINTING TRIANGLE
  , (0x201c, 0x12)  -- “ LEFT DOUBLE QUOTATION MARK
  , (0x201d, 0x13)  -- ” RIGHT DOUBLE QUOTATION MARK
  , (0x23EB, 0x14)  -- ⏫ BLACK UP-POINTING DOUBLE TRIANGLE
  , (0x23EC, 0x15)  -- ⏬ BLACK DOWN-POINTING DOUBLE TRIANGLE
  , (0x2022, 0x16)  -- • BULLET
  , (0x23CE, 0x17)  -- ⏎ RETURN SYMBOL
  , (0x2191, 0x18)  -- ↑ UPWARDS ARROW
  , (0x2193, 0x19)  -- ↓ DOWNWARDS ARROW
  , (0x2192, 0x1a)  -- → RIGHTWARDS ARROW
  , (0x2190, 0x1b)  -- ← LEFTWARDS ARROW
  , (0x2264, 0x1c)  -- ≤ LESS-THAN OR EQUAL TO
  , (0x2265, 0x1d)  -- ≥ GREATER-THAN OR EQUAL TO
  , (0x25B2, 0x1e)  -- ▲ BLACK UP-POINTING TRIANGLE
  , (0x25BC, 0x1f)  -- ▼ BLACK DOWN-POINTING TRIANGLE
  , (0x2302, 0x7f)  -- ⌂ HOUSE
  , (0x0411, 0x80)  -- Б CYRILLIC CAPITAL LETTER BE
  , (0x0414, 0x81)  -- Д CYRILLIC CAPITAL LETTER DE
  , (0x0416, 0x82)  -- Ж CYRILLIC CAPITAL LETTER ZHE
  , (0x0417, 0x83)  -- З CYRILLIC CAPITAL LETTER ZE
  , (0x0418, 0x84)  -- И CYRILLIC CAPITAL LETTER I
  , (0x0419, 0x85)  -- Й CYRILLIC CAPITAL LETTER SHORT I
  , (0x041B, 0x86)  -- Л CYRILLIC CAPITAL LETTER EL
  , (0x041F, 0x87)  -- П CYRILLIC CAPITAL LETTER PE
  , (0x0423, 0x88)  -- У CYRILLIC CAPITAL LETTER U
  , (0x0426, 0x89)  -- Ц CYRILLIC CAPITAL LETTER TSE
  , (0x0427, 0x8a)  -- Ч CYRILLIC CAPITAL LETTER CHE
  , (0x0428, 0x8b)  -- Ш CYRILLIC CAPITAL LETTER SHA
  , (0x0429, 0x8c)  -- Щ CYRILLIC CAPITAL LETTER SHCHA
  , (0x042A, 0x8d)  -- Ъ CYRILLIC CAPITAL LETTER HARD SIGN
  , (0x042B, 0x8e)  -- Ы CYRILLIC CAPITAL LETTER YERU
  , (0x042D, 0x8f)  -- Э CYRILLIC CAPITAL LETTER E
  , (0x03B1, 0x90)  -- α GREEK SMALL LETTER ALPHA
  , (0x266A, 0x91)  -- ♪ EIGHTH NOTE
  , (0x0393, 0x92)  -- Γ GREEK CAPITAL LETTER GAMMA
  , (0x03C0, 0x93)  -- π GREEK SMALL LETTER PI
  , (0x03A3, 0x94)  -- Σ GREEK CAPITAL LETTER SIGMA
  , (0x03C3, 0x95)  -- σ GREEK SMALL LETTER SIGMA
  , (0x266C, 0x96)  -- ♬ BEAMED SIXTEENTH NOTES
  , (0x03C4, 0x97)  -- τ GREEK SMALL LETTER TAU
  , (0x1F514, 0x98) -- 🔔 BELL
  , (0x0398, 0x99)  -- Θ GREEK CAPITAL LETTER THETA
  , (0x03A9, 0x9a)  -- Ω GREEK CAPITAL LETTER OMEGA
  , (0x03B4, 0x9b)  -- δ GREEK SMALL LETTER DELTA
  -- can't tell what 0x9c is supposed to be
  , (0x2665, 0x9d)  -- ♥ BLACK HEART SUIT
  , (0x03B5, 0x9e)  -- ε GREEK SMALL LETTER EPSILON
  , (0x2229, 0x9f)  -- ∩ INTERSECTION
  , (0x2016, 0xa0)  -- ‖ DOUBLE VERTICAL LINE
  ]

hashA02 :: EncodingHash
hashA02 = mkTable tableA02 $ [0x20..0x7e] ++ [0xa1..0xff]

hashTables :: [(RomCode, EncodingHash)]
hashTables = [(RomA00, hashA00), (RomA02, hashA02)]

mkTable :: [(Int, Word8)] -> [Word8] -> EncodingHash
mkTable table identityChars =
  H.fromList $ map (first chr) table ++ map f identityChars
  where f c = (chr $ fromIntegral c, c)

-- supportedChars :: Lcd -> [(Char, CharStatus)]

unicodeToByte :: CharEncoding -> Char -> Maybe Word8
unicodeToByte ce c =
  case c `elemIndex` ceCustomMapping ce of
    (Just i) -> Just (fromIntegral i)
    Nothing -> H.lookup c (ceBuiltIn ce)

ff :: (Int, [(Int, Int)]) -> [Bool] -> (Int, [(Int, Int)])
ff (len, spans) bools =
  let myLen = length bools
      polarity = head bools
      spans' = if polarity
               then spans -- new bytes and old bytes are equal
               else (len, myLen) : spans
      len' = len + myLen
  in (len', spans')

extractBytes :: B.ByteString -> (Int, Int) -> (Int, B.ByteString)
extractBytes bs (col, len) = (col, subStr)
  where subStr = B.take len $ B.drop col bs

findSpans :: B.ByteString -> B.ByteString -> [(Int, B.ByteString)]
findSpans old new =
  let bitMap = zipWith (==) (B.unpack old) (B.unpack new)
      grp = group bitMap
      pairs = snd $ foldl' ff (0, []) grp
  in sort $ map (extractBytes new) pairs

addLine :: [(Int, B.ByteString)] -> Int -> [(Int, Int, B.ByteString)]
addLine spans line = map f spans
  where f (col, bs) = (line, col, bs)

bytesToSpans :: [B.ByteString] -> [B.ByteString] -> [(Int, Int, B.ByteString)]
bytesToSpans old new =
  let spans = zipWith findSpans old new
      spans' = zipWith addLine spans [0..]
  in concat spans'

ensureLength :: LcdOptions -> [T.Text] -> [T.Text]
ensureLength lo ls = map ensureCols $ take numLines $ ls ++ repeat T.empty
  where
    ensureCols = padLine numColumns
    numLines = loLines lo
    numColumns = loColumns lo

txtToBs :: CharEncoding -> T.Text -> B.ByteString
txtToBs ce txt = B.pack $ map (fromMaybe 0x3f . unicodeToByte ce) $ T.unpack txt

-- | Updates the contents of the LCD.  You must specify the full contents
-- of the screen, but only the parts which have changed since the last update
-- are sent to the hardware.  Converts from Unicode to the display's
-- internal encoding, and automatically creates custom characters for
-- characters which are not directly supported by the LCD.
updateDisplay :: Lcd -> [T.Text] -> IO ()
updateDisplay lcd newText = do
  let cc = getCustomChars lcd $ concatMap T.unpack newText
  cm <- writeCustomChars lcd cc
  let ce = (lcdEncoding lcd) { ceCustomMapping = cm }
  updateDisplay' lcd ce newText

updateDisplay' :: Lcd -> CharEncoding -> [T.Text] -> IO ()
updateDisplay' lcd ce newTxt = do
  oldBs <- readIORef (lcdLines lcd)
  let newTxt' = rearrange $ ensureLength (lcdOptions lcd) newTxt
      newBs = map (txtToBs ce) newTxt'
      spans = bytesToSpans oldBs newBs
  forM_ spans $ \(line, col, bs) ->
    lcdWrite (lcdCb lcd) (fromIntegral line) (fromIntegral col) bs
  writeIORef (lcdLines lcd) newBs

-- Convert multiple lines to 2 or fewer lines.
-- In a 4-line display, lines 1 and 3 are treated as a single line,
-- and lines 2 and 4 are treated as a single line.
rearrange :: Monoid a => [a] -> [a]
rearrange [] = []
rearrange [x] = [x]
rearrange xs = [l1, l2]
  where (l1, l2) = f xs
        f [] = (mempty, mempty)
        f [y] = (y, mempty)
        f (y1:y2:rest) =
          let (z1, z2) = f rest
          in (y1 <> z1, y2 <> z2)

-- | Given callbacks and options, creates an 'Lcd'.  Assumes the display
-- has already been initialized via a call to 'lcdInitialize'.
mkLcd :: LcdCallbacks -> LcdOptions -> IO Lcd
mkLcd cb lo = do
  let ls = rearrange $ replicate (loLines lo) $ B.replicate (loColumns lo) 0x20
      nonChar = chr 0xffff -- a noncharacter according to Unicode standard
  ref <- newIORef ls
  cust <- newIORef (0, replicate 8 (nonChar, 0))
  let (Just builtIn) = loRomCode lo `lookup` hashTables -- should be safe
      ce = CharEncoding
           { ceBuiltIn = builtIn
           , ceCustom = loCustomChars lo
           , ceCustomMapping = [] -- unused in this context
           }
  return $ Lcd lo cb ref cust ce

data CharStatus = CharBuiltin   -- ^ character is supported by the LCD's ROM
                | CharCustom    -- ^ not supported by ROM, but available in
                                -- <https://www.cl.cam.ac.uk/~mgk25/ucs-fonts.html 5x8 fixed font>,
                                -- or in the user-defined characters specified
                                -- in 'loCustomChars'
                | CharNotFound  -- ^ not available in ROM, 5x8 fixed font, or
                                -- 'loCustomChars'
                deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | Given a Unicode code point, determines whether the character is
-- built-in, or is considered a custom character (of which only eight can
-- be on the screen at any one time).
getCharStatus :: Lcd -> Char -> CharStatus
getCharStatus lcd c =
  let ce = lcdEncoding lcd
      user = c `lookup` ceCustom ce
      builtIn = c `H.lookup` ceBuiltIn ce
      inFont = getCharacter c
  in case user of
    (Just _) -> CharCustom
    Nothing ->
      case builtIn of
        (Just _) -> CharBuiltin
        Nothing ->
          case inFont of
            (Just _) -> CharCustom
            Nothing -> CharNotFound

getCharData :: Lcd -> Char -> Maybe [Word8]
getCharData lcd c =
  let ce = lcdEncoding lcd
      user = c `lookup` ceCustom ce
  in case user of
    (Just _) -> user
    Nothing -> getCharacter c

getCustomChars :: Lcd -> String -> String
getCustomChars lcd str =
  sort $ nub $ filter (\x -> getCharStatus lcd x == CharCustom) str

matchExistingChars :: [(Char, Integer)] -> String -> (String, [(Int, Integer)])
matchExistingChars cust chars =
  let pairs = zip [0..] cust
      existing = map fst cust
      common = existing `intersect` chars
      chars' = chars \\ common
      pairs' = filter ff pairs
      ff (_, (c, _)) = c `notElem` common
      f (pos, (_, generation)) = (pos, generation)
  in (chars', map f pairs')

allocateCustomChars :: CustomInfo -> String -> CustomInfo
allocateCustomChars ci chars =
  let (chars', available) = matchExistingChars (snd ci) chars
      available' = map fst $ sortBy (comparing snd) available
      pairs = zip available' chars'
      generation = 1 + fst ci
      newStuff = zipWith replace [0..] (snd ci)
      replace i old@(c, _) = case i `lookup` pairs of
                               Nothing -> if c `elem` chars
                                          then (c, generation)
                                          else old
                               (Just c') -> (c', generation)
  in (generation, newStuff)

writeCustomChars :: Lcd -> String -> IO [Char]
writeCustomChars lcd chars = do
  let ref = lcdCustom lcd
  ci <- readIORef ref
  let ci' = allocateCustomChars ci chars
      oldNew = zip3 [0..] (map fst $ snd ci) (map fst $ snd ci')
  writeIORef ref ci'
  forM oldNew $ \(i, old, new) -> do
    when (old /= new) $ do
      let (Just cd) = getCharData lcd new -- this should be safe
      lcdDefineChar (lcdCb lcd) i cd
    return new
