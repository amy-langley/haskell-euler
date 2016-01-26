square :: Int -> Int
square x = x * x

squares :: Int -> [Int]
squares a = map square [1..2*a]

mySquares :: [Int]
mySquares = squares 1000

pairs :: [(Int, Int)]
pairs = [ (a, b) | a <- [1..998], b <- [1..998]]

testPair :: (Int, Int) -> Bool
testPair (a, b) = (square a + square b) `elem` mySquares

computeC :: Int -> Int -> Int
computeC a b = floor . sqrt . fromIntegral $ (square a + square b)

makeTuple :: (Int, Int) -> (Int, Int, Int)
makeTuple (a, b) = (a, b, computeC a b)

candidates :: [(Int, Int, Int)]
candidates = map makeTuple [ x | x <- pairs, testPair x ]

results :: [Int]
results = [ a * b * c | (a, b, c) <- candidates, a + b + c == 1000]

main :: IO ()
main = print $ head results
