import Data.List

data Expr a = Val a | Add (Expr a) (Expr a) | Sub (Expr a) (Expr a) | Mul (Expr a) (Expr a) | Div (Expr a) (Expr a) deriving Eq

showExpr (Val e) = show e
showExpr e = "(" ++ show e ++ ")"

instance (Show a) => Show (Expr a) where
  show (Val n) = show n
  show (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
  show (Sub e1 e2) = showExpr e1 ++ "-" ++ showExpr e2
  show (Mul e1 e2) = showExpr e1 ++ "*" ++ showExpr e2
  show (Div e1 e2) = showExpr e1 ++ "/" ++ showExpr e2


eval :: Integral a => Expr a -> a
eval (Val n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

isValid :: Integral a => Expr a -> Bool
isValid (Div _ (Val 0)) = False
isValid (Div (Val v1) (Val v2)) = v1 `mod` v2 == 0
isValid _ = True

ops :: [Expr a -> Expr a -> Expr a]
ops = [Add, Sub, Mul, Div]

genListsByDroppingOneElem :: [Int] -> [[Int]]
genListsByDroppingOneElem xs = map (\idx -> take (idx - 1) xs ++ drop idx xs) [1..length xs]

genPermutes :: [Int] -> [[Int]]
genPermutes [] = [[]]
genPermutes xs = filter ( not . null ) $ permutes ++ restPermutes
                 where permutes = permutations xs
                       restPermutes = concat $ map genPermutes (genListsByDroppingOneElem xs)

genSplits :: [a] -> [([a], [a])]
genSplits [] = []
genSplits [_] = []
genSplits (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- genSplits xs]

genExprs :: [Int] -> [Expr Int]
genExprs [] = []
genExprs [n] = [Val n]
genExprs xs = [ op l r | (ls, rs) <- genSplits xs, l <- genExprs ls, r <- genExprs rs, op <- ops, isValid $ op l r ]

solve :: [Int] -> Int -> [Expr Int]
solve nums expected = filter ( (==expected) . eval ) (concat $ map genExprs $ genPermutes nums)

input = [4, 0, 0]
expected = 4

--input = [1, 3, 7, 10, 25, 50]
--expected = 765

result = solve input expected
