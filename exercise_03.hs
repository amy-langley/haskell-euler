sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve [ n | n <- xs, n `mod` x > 0]

primes :: [Integer]
primes = sieve [2..]

big :: Integer
big = 600851475143

candidates :: [Integer]
candidates = takeWhile  ( < big ) primes

main :: IO ()
main = print $ last $ [ x | x <- candidates, big `mod` x == 0 ]
