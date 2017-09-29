import Data.List
import Data.Maybe

data Expr a = Val a | Add (Expr a) (Expr a) | Sub (Expr a) (Expr a) | Mul (Expr a) (Expr a) | Div (Expr a) (Expr a) deriving Eq

showExpr (Val e) = show e
showExpr e = "(" ++ show e ++ ")"

instance (Show a) => Show (Expr a) where
  show (Val n) = show n
  show (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
  show (Sub e1 e2) = showExpr e1 ++ "-" ++ showExpr e2
  show (Mul e1 e2) = showExpr e1 ++ "*" ++ showExpr e2
  show (Div e1 e2) = showExpr e1 ++ "/" ++ showExpr e2

evalOp :: Integral a => (a -> a -> a) -> Expr a -> Expr a -> Maybe a
evalOp op e1 e2 = do
  r1 <- eval e1
  r2 <- eval e2
  return $ op r1 r2

eval :: Integral a => Expr a -> Maybe a
eval (Val n) = Just n
eval (Add e1 e2) = evalOp (+) e1 e2
eval (Sub e1 e2) = evalOp (-) e1 e2
eval (Mul e1 e2) = evalOp (*) e1 e2
eval (Div e1 e2) = case (eval e1, eval e2) of
                     (_, (Just 0)) -> Nothing
                     ((Just x), (Just y)) -> if x `mod` y == 0 then Just $ x `div` y else Nothing
                     _ -> Nothing

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
genExprs xs = [ op l r | (ls, rs) <- genSplits xs, l <- genExprs ls, r <- genExprs rs, op <- ops ]

solve :: [Int] -> Int -> [Expr Int]
solve nums expected = filter isSolution (concat $ map genExprs $ genPermutes nums)
                      where isSolution e = case eval e of
                                           Nothing -> False
                                           (Just v) -> v == expected

input = [1, 3, 7, 10, 25, 50]
expected = 765

result = solve input expected
