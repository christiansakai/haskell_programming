import Data.Char (ord, chr, isLetter)

type Original = Char
type Key      = Char
type Encoded  = Char

vigenere :: [Original] -> [Key] -> [Encoded]
vigenere sentence keyword = map encode $ zipOriginalWithKey sentence keyword

zipOriginalWithKey :: [Original] -> [Key] -> [(Original, Key)]
zipOriginalWithKey original key = goZip original key
  where goZip ""       _      = []
        goZip original ""     = goZip original key
        goZip (' ':os) ks     = (' ', ' ') : goZip os ks
        goZip (o:os)   (k:ks) = (o, k) : goZip os ks

encode :: (Original, Key) -> Encoded
encode (' ', ' ')       = ' '
encode (char, keyChar)  = chr $ (ord char + (ord keyChar - ord 'A') - ord 'A') `mod` 26 + ord 'A' 

decode :: (Encoded, Key) -> Original
decode (' ', ' ')       = ' '
decode (char, keyChar)  = chr $ (ord char - (ord keyChar - ord 'A') - ord 'A') `mod` 26 + ord 'A' 

unvigenere :: [Encoded] -> [Key] -> [Original]
unvigenere sentence keyword = map decode $ zipOriginalWithKey sentence keyword
 
testSentence = "MATT AT DAWN"
testKey      = "ALLY"

testVigenered = vigenere testSentence testKey
testUnvigenered = unvigenere testVigenered testKey

main = 
  if testSentence == testUnvigenered 
     then putStrLn "Cipher ok!"
     else putStrLn "Cipher NOT ok!"
