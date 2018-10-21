module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)



data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]



lookUp :: Eq a => a -> [(a, b)] -> b
lookUp s l
  = fromJust (lookup s l)


--kept these earlier attempts in so I can see my mistakes more clearly
  --eval expr env
  --  | elem a expr = eval
  --      where (a, _) = concat env
  --
  --eval expr env
  --  | expr == Double = expr
  --  | expr == String = lookUp expr env
  --  | otherwise = error
  --
  --eval expr env
  --  | expr == (Val doub) = doub
  --  | expr == (Id str) = lookUp str env
  --  | otherwise = error
  --
--created these lists to simplify my code
unOpMap = [(Neg, (*(-1))), (Sin, (sin)), (Cos, (cos)), (Log, (log))]
binOpMap = [(Add, (+)), (Mul, (*)), (Div, (/))]

eval :: Exp -> Env -> Double
eval (Val doub) env
  = doub
eval (Id str) env
  = lookUp str env
eval (UnApp uop expr) env
  = (lookUp uop unOpMap) (eval expr env)
eval (BinApp bop expr expr') env
  = (lookUp bop binOpMap) (eval expr env) (eval expr' env)


diff :: Exp -> String -> Exp
diff (Val doub) env
  = Val 0
diff (Id str) env
  = Val (if str == env then 1 else 0)
diff (UnApp Neg expr) env
  = UnApp Neg (diff expr env)
diff (UnApp Sin expr) env
  = BinApp Mul (UnApp Cos expr) (diff expr env)
diff (UnApp Cos expr) env
  = UnApp Neg (BinApp Mul (UnApp Sin expr) (diff expr env))
diff (UnApp Log expr) env
  = BinApp Div (diff expr env) expr
diff (BinApp Add expr expr') env
  = BinApp Add (diff expr env) (diff expr' env)
diff (BinApp Mul expr expr') env
  = BinApp Add (BinApp Mul expr (diff expr' env)) (BinApp Mul (diff expr env) expr')
diff (BinApp Div expr expr') env
  = BinApp Div (BinApp Add (BinApp Mul (diff expr env) expr') (UnApp Neg (BinApp Mul expr (diff expr' env)))) (BinApp Mul expr' expr')
--make sure product rule is correct way round (may fail test if wrong way round)
--wasn't sure how to implement this "case () of _ syntax" I found online
--diff (UnApp uop expr) env
--  = case uop of
--    Neg -> Val 0.0
--    Sin -> Val (cos expr)
--    Cos -> Val ((-1) * sin expr)
--    Log -> Val (1/expr)
--    otherwise -> error "Unary Operation not defined."

maclaurin :: Exp -> Double -> Int -> Double
maclaurin expr a n = foldl1 (+) terms
  where terms = take n (zipWith3 (\x y z -> x * y / z) powers differentials factorials)
        powers = iterate (*a) 1
        differentials = map (flip eval [("x",0)]) (iterate (flip diff "x") expr)
        factorials = scanl (*) 1 [1..]


showExp :: Exp -> String
showExp (Val x)
  = show x
showExp (Id x)
  = x
showExp (UnApp Neg x)
  = "-("++ showExp x ++")"
showExp (UnApp Cos x)
  = "cos("++ showExp x ++")"
showExp (UnApp Sin x)
  = "sin("++ showExp x ++")"
showExp (UnApp Log x)
  = "log("++ showExp x ++")"
showExp (BinApp Add x y)
  = "(" ++ showExp x ++ "+" ++ showExp y ++ ")"
showExp (BinApp Mul x y)
  = "(" ++ showExp x ++ "*" ++ showExp y ++ ")"
showExp (BinApp Div x y)
  = "(" ++ showExp x ++ "/" ++ showExp y ++ ")"



---------------------------------------------------------------------------
-- Test cases from the spec.

e1, e2, e3, e4, e5, e6 :: Exp

-- > 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- > x*x + y - 7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- > x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- > -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- > sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- > log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))

----------------------------------------------------------------------
-- EXTENSION: Uncomment and complete these...

-- instance Num Exp where

-- instance Fractional Exp where

-- instance Floating Exp where


-- instance (Eq a, Num a) => Num (Maybe a) where

-- instance (Eq a, Fractional a) => Fractional (Maybe a) where

-- diff2 :: Exp -> String -> Maybe Exp



-- The following makes it much easier to input expressions, e.g. sin x, log(x*x) etc.

x, y :: Exp
x = Id "x"
y = Id "y"
