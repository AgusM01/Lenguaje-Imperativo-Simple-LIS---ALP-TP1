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
-- intexp ::= intexp '+' term | intexp '-' term | term
-- iterm   ::= iterm '*' minus | iterm '/' minus | minus  
-- minus  ::= '-u' mm | mm  
-- mm     ::= nat | var | var '++' | var '--' | '('intexp')'  

intexp :: Parser (Exp Int)
intexp =  chainl1 iterm sumRestParser -- chainl1 parsea AL MENOS 1, por lo que parseará term solo

iterm :: Parser (Exp Int)
iterm = chainl1 minus prodDivParser 

minus :: Parser (Exp Int)
minus = chainl1 mm uminParser 

mm :: Parser (Exp Int)
mm = try natParse <|> try varParse <|> try varPlusPlus <|> try varMinusMinus <|> parensParser

sumRestParser :: Parser (Exp Int -> Exp Int -> Exp Int)
sumRestParser = try (do reservedOp lis "+"
                        return (\x y -> Plus x y))
                    <|> do reservedOp lis "-"
                           return (\x y -> Minus x y)

prodDivParser :: Parser (Exp Int -> Exp Int -> Exp Int)
sumRestParser = try (do reservedOp lis "*"
                        return (\x y -> Times x y))
                    <|> do reservedOp lis "/"
                           return (\x y -> Div x y)

natParse :: Parser (Exp Int) 
natParse = do n <- natural lis
              return (Const n)


                   
varPlusPlus :: Parser (Exp Int)
varPlusPlus = do v <- identifier lis
                 reservedOp lis "+"
                 reservedOp lis "+"
                 return (VarInc v)

varMinusMinus :: Parser (Exp Int)
varMinusMinus = do v <- identifier lis
                   reservedOp lis "-"
                   reservedOp lis "-"
                   return (VarDec v)

parensParser :: Parser (Exp Int) 
parensParser =  do symbol "("
                   i <- intexp
                   symbol ")"
                   return i


-- En algun momento tendremos que parsear variables.
-- Utilizaremos la función identifier la cual le tenemos que pasar una identifiacion de token -> lis : v <- identifier lis
-- identifier toma la especificacion de tokens que da lis y generará un string. Aquí checkeara si por ejemplo la variable no es una palabra reservada.
-- Si identifier lee una de las palabras reservadas de lis, falla.

variableParser :: Parser (Exp Int)
variableParser = do v <- identifier lis 
                    return (Var v)

-- Parser de  - intexp
uminParser :: Parser (Exp Int -> Exp Int)
uminParser = do reservedOp lis  "-"
                return (\e -> UMinus e) 
------------------------------------
--- Parser de expresiones booleanas
------------------------------------

-- Desambigüamos la gramática
-- boolexp  ::= boolexp '||' band | band 
-- band     ::= band '&&' bnot | bnot 
-- bnot     ::= '!' bop | bop  
-- bop      ::= bop ('<' bdata | '>' bdata | '!=' bdata | '==' bdata) | bdata
-- bdata    ::= 'true' | 'false' 

boolexp :: Parser (Exp Bool)
boolexp = chainl1 band orParser

band :: Parser (Exp Bool) 
band = chainl1 bnot andParser

bnot :: Parser (Exp Bool) 
bnot = chainl1 bop notParser

bop :: Parser (Exp Bool)
bop = chainl1 bdata opParser

bdata :: Parser (Exp Bool)
bdata = try (reservedNames lis "true") <|> reservedNames lis "false"

-----------------------------------
--- Parser de comandos
-----------------------------------

-- Desambigüamos la gramática
-- comm     ::= comm ';' asig | asig 
-- asig     ::= var '=' intexp | control 
-- control  ::= 'skip' | ’if’ boolexp ’{’ comm ’}’| ’if’ boolexp ’{’ comm ’}’ ’else’ ’{’ comm ’}’ | ’repeat’ ’{’ comm ’}’ ’until’ boolexp


comm :: Parser Comm
comm = chainl1 asig semi -- parser de ';'

asig :: Parser Comm 
asig = try (do v <- varParse 
               reservedOp lis "=" 
               i <- intexp 
               return (Eq v i)) <|> control

control :: Parser Comm
control = (try reservedNames lis "skip") <|> (try ifThenParser) <|> (try ifElseParser) <|> repParser

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm -- Este es el parser que queremos definir.
parseComm = parse (totParser comm)
