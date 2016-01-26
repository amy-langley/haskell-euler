fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

evenFibs :: [Integer]
evenFibs = [ x | x <- fibs, x `mod` 2 == 0 ]

main :: IO ()
main = print $ sum $ takeWhile (<=4000000) evenFibs
