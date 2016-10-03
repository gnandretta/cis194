{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


import ExprT
import Parser
import StackVM 
import qualified Data.Map as M


-- Exercise 1

eval :: ExprT -> Integer
eval (ExprT.Lit number) = number
eval (ExprT.Add left right) = eval left + eval right
eval (ExprT.Mul left right) = eval left * eval right


-- eval (ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)) == 20


-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr exp =
  case parseExp ExprT.Lit ExprT.Add ExprT.Mul exp of
    (Just exprT) -> Just (eval exprT)
    Nothing -> Nothing


-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a


instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul


-- mul (add (lit 2) (lit 3)) (lit 4) :: ExprT == Mul (Add (Lit 2) (Lit 3)) (Lit 4)


-- Exercise 4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)


instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)


newtype MinMax = MinMax Integer deriving (Eq, Show)


instance Expr MinMax where
  lit = MinMax
  add (MinMax left) (MinMax right) = MinMax $ max left right
  mul (MinMax left) (MinMax right) = MinMax $ min left right  


newtype Mod7 = Mod7 Integer deriving (Eq, Show)


instance Expr Mod7 where
  lit = Mod7
  add (Mod7 left) (Mod7 right) = Mod7 $ (left + right) `mod` 7
  mul (Mod7 left) (Mod7 right) = Mod7 $ (left * right) `mod` 7


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer  -- -12 + 5 = -7
testBool    = testExp :: Maybe Bool     -- (True * False) + True = False + True = True
testMM      = testExp :: Maybe MinMax   -- -4 + 5 = 5
testSat     = testExp :: Maybe Mod7     -- -5 + 5 = 0


-- Exercise 5

instance Expr StackVM.Program where
  lit number = [StackVM.PushI number]
  add left right = left ++ right ++ [StackVM.Add]
  mul left right = left ++ right ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul


-- compile "3 * 6" == Just [PushI 3,PushI 6,Mul]


-- Exercise 6

class HasVars a where
  var :: String -> a


data VarExprT = Var String
              | Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
  deriving (Show, Eq)


instance HasVars VarExprT where
  var = Main.Var


instance Expr VarExprT where
  lit = Main.Lit
  add = Main.Add
  mul = Main.Mul


instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

-- NOTE: the type of M.lookup helps understand what's going on.
--
-- M.lookup :: Ord k => k -> Map k a -> Maybe a
--          :: String -> Map String Integer -> Maybe Integer


instance Expr (M.Map String Integer -> Maybe Integer) where
  lit number _ = Just number
  add left right map =
    case ((left map),(right map)) of
      ((Just a),(Just b)) -> Just (a + b)
      (_,_) -> Nothing
  mul left right map =
    case ((left map),(right map)) of
      ((Just a),(Just b)) -> Just (a * b)
      (_,_) -> Nothing


withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs


-- > :t add (lit 3) (var "x")
-- add (lit 3) (var "x") :: (Expr a, HasVars a) => a
--
-- (withVars [("x", 6)] $ add (lit 3) (var "x")) == Just 9
-- (withVars [("x", 6)] $ add (lit 3) (var "y")) == Nothing
-- (withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))) == Just 54
