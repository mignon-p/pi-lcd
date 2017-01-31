table :: [(Int, Word8)]
table =
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

supportedChars :: [Char]
supportedChars =
  map ord $ sort $ map fst table ++ [0x20..0x7e] ++ [0xa1..0xff]

unicodeToByte :: Int -> Maybe Word8
unicodeToByte c =
  if (c >= 0x20 && c <= 0x7e) || (c >= 0xa1 && c <= 0xff)
  then Just $ fromIntegral c
  else lookup c table
