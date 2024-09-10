module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a -- Se deshace de los espacios en blanco y, mediante lis, tokeniza la entrada.
totParser p = do
  whiteSpace lis -- 
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "++"
                        , "--"
                        ]
    }
  )

-- Me permite parsear en 2 pasos:
-- En el primer paso, transforma la cadena en una secuencia de tokens. Para esto busca las palabras
-- reservadas del lenguaje, numeros, operadores, y a eso le llama token. Todo lo que no le dijimos que 
-- es un token lo omite. Lidia con los comentarios tambien.
-- Para este primer paso, especifica un TokenParser. La función makeTokenParser nos pide que le digamos
-- cuales son los tokens. Me permite evitar que el usuario, por ejemplo, llame a una variable "skip" ya que 
-- le dijimos que la misma es una palabra reservada del lenguaje. Se podria pensar como que los tokens tienen "tipos". 

-- La gramática es ambigüa. La desambigüamos especificando orden de precedencia.
-- Esa gramática tendra el problema de la recursión a izquierda.
-- Para eso utilizaremos chainl1 :: ParserExp -> ParserSep -> Exp a 
-- chainl1 crea el Exp y ademas arregla la recursión a izquierda.
-- El orden de presendencia lo damos en el ParserSep. 
-----------------------------------
--- Parser de expresiones enteras
-----------------------------------

-- Desambigüamos la gramática:
-- intexp ::= intexp '+' term | intexp '-b' term | term
-- iterm   ::= iterm '*' minus | iterm '/' minus | minus  
-- minus  ::= '-u' mm | mm  
-- mm     ::= nat | var | var '++' | var '--' | '('intexp')'  

intexp :: Parser (Exp Int)
intexp =  chainl1 iterm sumRestParser -- chainl1 parsea AL MENOS 1, por lo que parseará term solo

iterm :: Parser (Exp Int)
iterm = chainl1 minus prodDivParser 

minus :: Parser (Exp Int)
minus = chainl1 mm uminParser 

--minus :: Parser (Exp Int)
--minus = try (do {reservedOp lis "-"; m <- mm; return (UMinus m)}) <|> (mm)

mm :: Parser (Exp Int)
mm = try natParse <|> try varParse <|> try varPlusPlus <|> try varMinusMinus <|> parensParser

            

-- En algun momento tendremos que parsear variables.
-- Utilizaremos la función identifier la cual le tenemos que pasar una identifiacion de token -> lis : v <- identifier lis
-- identifier toma la especificacion de tokens que da lis y generará un string. Aquí checkeara si por ejemplo la variable no es una palabra reservada.
-- Si identifier lee una de las palabras reservadas de lis, falla.

-- variableParser :: Parser (Exp Int)
-- variableParser = do v <- identifier lis 
--                     return (Var v)

-- Parser de  - intexp
-- uminParser :: Parser (Exp Int)
-- uminParser = do reservedOp lis  "-"
--                 e <- intextp
--                 return (UMinus e) 
------------------------------------
--- Parser de expresiones booleanas
------------------------------------

-- Desambigüamos la gramática
-- boolexp  ::= boolexp '||' band | band 
-- band     ::= band '&&' bnot | bnot 
-- bnot     ::= '!' bop | bop  
-- bop      ::= bop ('<' bdata | '>' bdata | '!=' bdata | '==' bdata) | bdata
-- bdata    ::= 'true' | 'false' 


orParser :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orParser = do {reservedOp lis "||"; return (\x y -> (Or x y))}
				
andParser :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
andParser = do {reservedOp lis "&&"; return (\x y -> (And x y))}
		
notParser :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
notParser = do {reservedOp lis "!"; x <- bop; return (Not x)}

opParser :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
opParser = try 	(reservedOp lis "<"; return (\x y -> (Lt x y))) <|>
		(try reservedOp lis ">"; return (\x y -> (Gt x y)) <|>
		(try reservedOp lis "=="; return (\x y -> (Eq x y)) <|>
		(try reservedOp lis "!="; return (\x y -> (NEq x y)))))

boolexp :: Parser (Exp Bool)
boolexp = chainl1 band orParser

band :: Parser (Exp Bool) 
band = chainl1 bnot andParser

bnot :: Parser (Exp Bool) 
bnot = try notParser <|> bop

bop :: Parser (Exp Bool)
bop = chainl1 bdata opParser

bdata :: Parser (Exp Bool)
bdata = try (reservedNames lis "true"; return BTrue) <|> (reservedNames lis "false"; return BFalse)

-----------------------------------
--- Parser de comandos
-----------------------------------

-- Desambigüamos la gramática
-- comm     ::= comm ';' asig | asig 
-- asig     ::= var '=' intexp | control 
-- control  ::= 'skip' | ’if’ boolexp ’{’ comm ’}’| ’if’ boolexp ’{’ comm ’}’ ’else’ ’{’ comm ’}’ | ’repeat’ ’{’ comm ’}’ ’until’ boolexp

ifThenParser :: Parser (Comm)
ifThenParser = 	do (reservedNames lis "if") 
			b <- boolexp
			symbol "{"
			c <- comm
			symbol "}"
			return (IfThenElse b c Skip) -- mirar en AST, hay un pattern
			 			
	   
ifElseParser :: Parser (Comm)
ifElseParser = do (reservedNames lis "if") 
			b <- boolexp
			symbol "{" 
			c1 <- comm
			symbol "}"
			symbol "{"
			c2 <- comm
			symbol "}"
			return (IfThenElse b c1 c2) 

repParser :: Parser (Comm)
repParser = do (reservedNames lis "repeat")
		symbol "{" 
		c <- comm
		symbol "}"
		reservedNames lis "until"
		b <- boolExp
		return (RepeatUntil c b)
		
skipParser :: Parser (Comm)
skipParser = do {reservedNames lis "skip"; return Skip}

semiParser :: Parer (Comm -> Comm -> Comm)
semiParser = do {reservedOp lis ";"; return (\ c1 c2 -> (Seq c1 c2))}

comm :: Parser Comm
comm = chainl1 asig semiParser

asig :: Parser Comm 
asig = try (do v <- varParse 
               reservedOp lis "=" 
               i <- intexp 
               return (Let v i)) <|> control  

control :: Parser Comm
control = try (skipParser) <|> (try (ifThenParser) <|> (try (ifElseParser) <|> repParser))

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm -- Este es el parser que queremos definir.
parseComm = parse (totParser comm)
