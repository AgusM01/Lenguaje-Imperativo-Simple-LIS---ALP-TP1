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
intexp :: Parser (Exp Int)
intexp = undefined

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

boolexp :: Parser (Exp Bool)
boolexp = undefined

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = undefined

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm -- Este es el parser que queremos definir.
parseComm = parse (totParser comm)
