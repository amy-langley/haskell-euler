fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = print $ foldl (+) 0 $ take 50 fibs
