square :: Integer -> Integer
square x = x * x

main :: IO ()
main = print $ (square $ sum [1..100]) - (sum $ map square [1..100])
