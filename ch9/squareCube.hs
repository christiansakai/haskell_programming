mySqr = [x ^ 2 | x <- [1..5]]
myCube = [y ^ 3 | y <- [1..5]]

one :: [(Int, Int)]
one = [(x, y) | x <- mySqr, y <- myCube]

two :: [(Int, Int)]
two = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

three :: Int
three = length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
