{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple             as T 
import           Prelude                       as P 

-- Estados  
type State = (M.Map Variable Int, String)

-- Estado vacío
-- Completar la definición
initState :: State
initState = (M.empty, "")

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v (P.fst s) of
                    Just a -> Right a
                    Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v i s = (M.insert v i (P.fst s), (P.snd s))

-- Agrega una traza dada al estado
-- Completar la definición
addTrace :: String -> State -> State
addTrace str s = (P.fst s, (P.snd s) ++ str)

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm c s =  case c of
                Skip ->     Right(Skip :!: addTrace "Skip" s)
                Let v e ->  case (evalExp e s) of
                            Left err -> Left err
                            Right ex -> Right(Skip :!: addTrace ("Let "++v++" "++(show (T.fst ex)++ "||"))  
                                                                (update v (T.fst ex) s))

                
                Seq c1 c2 -> if c1 == Skip then  Right(c2 :!: s)
                              else  case (stepComm c1 s) of
                                    Left err -> Left err
                                    Right t -> Right((Seq (T.fst t) c2) :!: (T.snd t))
                                    
                IfThenElse e c1 c2 -> case (evalExp e s) of
                                      Left err -> Left err
                                      Right ev -> if T.fst ev then Right(c1 :!: T.snd ev) 
                                                              else Right(c2 :!: T.snd ev)
                RepeatUntil r e -> Right(Seq r (IfThenElse e Skip (RepeatUntil r e)) :!: s) 
                
-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp e s = case e of 
                    Const i   ->  Right(i :!: s) 
                    Var v     ->  case lookfor v s of
                                  Left err -> Left err
                                  Right i -> Right(i :!: s)
                    UMinus i  ->  case evalExp i s of
                                  Left err -> Left err
                                  Right ev -> Right(-(T.fst ev) :!: T.snd ev) 
                    Plus x y  ->  case evalExp x s of
                                  Left err -> Left err
                                  Right (e' :!: s') ->  case evalExp y s' of
                                                        Left err -> Left err
                                                        Right(e'' :!: s'') -> Right((e' + e'') :!: s'')
                    Minus x y ->  case evalExp x s of
                                  Left err -> Left err
                                  Right (e' :!: s') ->  case evalExp y s' of
                                                        Left err -> Left err
                                                        Right(e'' :!: s'') -> Right((e' - e'') :!: s'')
                    Times x y ->  case evalExp x s of
                                  Left err -> Left err
                                  Right (e' :!: s') ->  case evalExp y s' of
                                                        Left err -> Left err
                                                        Right(e'' :!: s'') -> Right((e' * e'') :!: s'')
                    Div x y   ->  case evalExp x s of
                                  Left err -> Left err
                                  Right (e' :!: s') ->  case evalExp y s' of
                                        Left err -> Left err
                                        Right(e'' :!: s'') -> if e'' == 0 then Left(DivByZero) 
                                                                          else Right((div e' e'') :!: s'')
                    VarInc v  ->  case lookfor v s of
                                  Left err  -> Left err
                                  Right x   -> Right (x + 1 :!: addTrace ("Let "++v++" "++(show (x + 1)) ++ "||")  
                                                                (update v (x+1) s))
                    VarDec v  ->  case lookfor v s of
                                  Left err -> Left err
                                  Right x -> Right(x - 1 :!: addTrace ("Let "++v++" "++(show (x - 1)) ++ "||")  
                                                                (update v (x - 1) s))
                    BTrue     -> Right(True :!: s) 
                    BFalse    -> Right(False :!: s)
                    Lt x y    -> case evalExp x s of
                                  Left err -> Left err
                                  Right (e' :!: s') ->  case evalExp y s' of
                                                        Left err -> Left err
                                                        Right(e'' :!: s'') -> Right((e' < e'') :!: s'')
                    Gt x y    -> case evalExp x s of
                                  Left err -> Left err
                                  Right (e' :!: s') ->  case evalExp y s' of
                                                        Left err -> Left err
                                                        Right(e'' :!: s'') -> Right((e' > e'') :!: s'')
                    And x y   -> case evalExp x s of
                                  Left err -> Left err
                                  Right (e' :!: s') ->  case evalExp y s' of
                                                        Left err -> Left err
                                                        Right(e'' :!: s'') -> Right((e' && e'') :!: s'')
                    Or x y    -> case evalExp x s of
                                  Left err -> Left err
                                  Right (e' :!: s') ->  case evalExp y s' of
                                                        Left err -> Left err
                                                        Right(e'' :!: s'') -> Right((e' || e'') :!: s'')
                    Not x     ->  case evalExp x s of
                                  Left err -> Left err
                                  Right ev -> Right(not(T.fst ev) :!: T.snd ev)
                    Eq x y    -> case evalExp x s of
                                  Left err -> Left err
                                  Right (e' :!: s') ->  case evalExp y s' of
                                                        Left err -> Left err
                                                        Right(e'' :!: s'') -> Right((e' == e'') :!: s'')
                    NEq x y   -> case evalExp x s of
                                  Left err -> Left err
                                  Right (e' :!: s') ->  case evalExp y s' of
                                                        Left err -> Left err
                                                        Right(e'' :!: s'') -> Right((e' /= e'') :!: s'')