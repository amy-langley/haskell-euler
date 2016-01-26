sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve [ n | n <- xs, n `mod` x > 0]

primes :: [Integer]
primes = sieve [2..]

big :: Integer
big = 600851475143
-- big = 13195

candidates :: Integer -> [Integer]
candidates upto = takeWhile  ( < upto ) primes

removePrimes :: Integer -> Integer -> [Integer] -> Integer
removePrimes largest _ [] = largest
removePrimes largest factorme (p:ps) =
  if largest > factorme then largest
  else if factorme `mod` p == 0 then (removePrimes p (factorme `div` p) (p:ps))
  else removePrimes largest factorme ps

factor :: Integer -> Integer
factor x = removePrimes 1 x $ candidates x

main :: IO ()
main = print $ factor big
