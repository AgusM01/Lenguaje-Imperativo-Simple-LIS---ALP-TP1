module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple             as T

-- Estados
type State = M.Map Variable Int -- Las keys son variables, los valores Int.

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty 

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v s = case M.lookup v s of
                    Just a -> a
                    Nothing -> error "Variable no encontrada"
              

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v i s = M.adjust (\x -> i) v s  

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = T.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm c s = case c of
                    Skip ->     Skip :!: s
                    Let v e ->  Skip :!: update v (T.fst (evalExp e s)) s
                    Seq c1 c2 -> if c1 == Skip then  c2 :!: s
                                  else  let t = (stepComm c1 s)
                                        in Seq (T.fst t) c2 :!: (T.snd t)
                    IfThenElse e c1 c2 -> let ev = (evalExp e s)  
                                          in if T.fst ev then c1 :!: T.snd ev 
                                              else c2 :!: T.snd ev
                    RepeatUntil c e -> Seq c (IfThenElse e Skip (RepeatUntil c e)) :!: s 
                                      
-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp e s = case e of 
                    Const i   -> i :!: s 
                    Var v     -> lookfor v s :!: s
                    UMinus i  ->  let ev = evalExp i s
                                  in -(T.fst ev) :!: T.snd ev 
                    Plus x y  ->  f x y (+)
                    Minus x y ->  f x y (-)
                    Times x y ->  f x y (*)
                    Div x y   ->  f x y (/)
                    VarInc v  ->  let x = lookfor v s
                                  in x + 1 :!: update v (x + 1) s
                    VarDec v  ->  let x = lookfor v s
                                  in x - 1 :!: update v (x - 1) s
                    BTrue     -> True :!: s 
                    BFalse    -> False :!: s 
                    Lt x y    -> f x y (<)
                    Gt x y    -> f x y (>)
                    And x y   -> f x y (&&) 
                    Or x y    -> f x y (||)
                    Not x     ->  let ev = evalExp x s
                                  in not(T.fst ev) :!: T.snd ev
                    Eq x y    -> f x y (==)
                    NEq x y   -> f x y (!=)
                    

                where f x y op =  let e' :!: s' = evalExp x s
                                      e'' :!: s'' = evalExp y s'
                                  in e' op e'' :!: s''

g :: (Num a, Ord a, Eq a) => a -> a -> State -> (a -> a) -> Pair a State
g x y op s  =   let e' :!: s' = evalExp x s
                    e'' :!: s'' = evalExp y s'
                in e' op e'' :!: s''