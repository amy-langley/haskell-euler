import           Data.List

multiples :: Integer -> [Integer] -> [Integer]
multiples n ns = [ x | x <- ns, x `mod` n == 0]

mult3 :: [Integer]
mult3 = multiples 3 [1..999]
mult5 :: [Integer]
mult5 = multiples 5 [1..999]

mults :: [Integer]
mults = mult3 `union` mult5

main :: IO ()
main = print $ sum mults
