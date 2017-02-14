stops = "pbtdkg"
vowels = "aeiou"

createThreeTupleCombo :: String -> String -> [(Char, Char, Char)]
createThreeTupleCombo ss vs = [(s, v, s') | s <- ss, v <- vs, s' <- ss]

createThreeTupleCombo' :: String -> String -> [(Char, Char, Char)]
createThreeTupleCombo' ss vs = [(s, v, s') | s <- ss, v <- vs, s' <- ss, s == 'p']

nouns = ["table"
        ,"desk"
        ,"laptop"
        ,"monitor"
        ,"printer"
        ,"chair"]

verbs = ["type"
        ,"sit"
        ,"read"
        ,"use"
        ,"drink"]

createThreeTupleCombo'' :: [String] -> [String] -> [(String, String, String)]
createThreeTupleCombo'' ns vs = [(n, v, n) | n <- ns, v <- vs, n <- ns]

seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x)))
                    (length (words x))

seekritFunc' :: (Fractional a) => String -> a
seekritFunc' x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))
