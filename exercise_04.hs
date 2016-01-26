import Data.Bool

products :: [Integer]
products = [ x * y | x <- [100..999], y <- [100..999]]

palindromic :: String -> Bool
palindromic x = x == reverse x

palindromes :: [Integer]
palindromes = [ x | x <- products, palindromic $ show x ]

main :: IO ()
main = print $ maximum palindromes
