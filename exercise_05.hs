import Data.Bool

nums :: [Integer]
nums = [20..]

divisors :: [Integer]
divisors = [2..20]

divisible :: Integer -> Integer -> Bool
divisible x y = x `mod` y == 0

check :: Integer -> Bool
check x = all (divisible x) divisors

candidates :: [Integer]
candidates = [ x | x <- nums, check x ]

main :: IO ()
main = print $ head candidates
