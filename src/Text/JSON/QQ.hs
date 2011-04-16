{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell -XQuasiQuotes -XUndecidableInstances #-}

-- | This package expose the function @jsonQQ@ that compile time converts json code into a @Text.JSON.JSValue@.
--    @jsonQQ@ got the signature
--    
--    > jsonQQ :: QuasiQuoter
--    
--    and is used like
--    
--    > myCode = [jsonQQ| {age: 23, name: "Pelle", likes: ["mac","Haskell"] } |]
--    
--    where it is important that
--    
--    * you got no space in @[jsonQQ|@ and
--    
--    * no additional code after @|]@.
--    
--    The quasiquatation can also bind to variables like
--    
--    > myCode = [jsonQQ| {age: <|age|>, name: <|name|>} |]
--    > where age = 34 :: Integer
--    >       name = "Pelle"
--    
--    where the function  @toJSON@ will be called on @age@ and @name@ runtime.
--
--    You can also insert Haskell code: 
--
--    > myCode = [jsonQQ| {age: <|age + 34 :: Integer|>, name: <|map toUpper name|>} |]
--    > where age = 34 :: Integer
--    >       name = "Pelle"
--    
--    You can use a similar syntax if you want to insert a value of type JSValue like
--    
--    > myCode = [jsonQQ| {"age": <<age>>} |]
--    
--    If you want to replace the name of the key in a hash you'll use the $-syntax:
--    
--    > foo = [jsonQQ| {$bar: 42} |]
--    > bar = "age"
--    

module Text.JSON.QQ (
  jsonQQ
) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Data
import Data.Maybe

import Text.JSON
import Text.JSON.Generic

import Data.Ratio
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
-- import Text.Parsec.Perm

import Language.Haskell.Meta.Parse

jsonQQ :: QuasiQuoter
jsonQQ = QuasiQuoter { quoteExp = jsonExp, quotePat = jsonPat}


jsonExp :: String -> ExpQ
jsonExp txt =
  case parsed' of 
    Left err -> error $ "Error in jsonExp: " ++ show err
    Right val -> return $ toExp val
  where
    parsed' = parse jpValue "txt" txt

jsonPat :: String -> PatQ
jsonPat s = undefined


----
-- JSValue etc to ExpQ
---------

class ToExp a where
  toExp :: a -> Exp

instance ToExp JsonValue where
  toExp (JsonString str) = 
    AppE (ConE $ mkName "Text.JSON.JSString") (AppE (VarE $ mkName "Text.JSON.toJSString") (LitE (StringL $ str)))

  toExp (JsonNull) = ConE $ mkName "Text.JSON.Types.JSNull"

  toExp (JsonObject objs) = 
    AppE (ConE $ mkName "Text.JSON.JSObject") (AppE (VarE $ mkName "Text.JSON.toJSObject") (ListE $ jsList ))
    where
      jsList :: [Exp] -- [(String,JSValue)]
      jsList = map objs2list (objs)
      objs2list :: (HashKey,JsonValue) -> Exp
      objs2list (HashStringKey k,v) = TupE [LitE (StringL k), toExp v]
      objs2list (HashVarKey k,v) = TupE [VarE $ mkName k, toExp v]

  toExp (JsonArray arr) =
    AppE (ConE $ mkName "Text.JSON.JSArray") (ListE $ map toExp arr)

  toExp (JsonNumber b rat) =
    AppE (AppE (ConE $ mkName "Text.JSON.JSRational") (ConE $ mkName (if b then "True" else "False"))) (InfixE (Just (LitE (IntegerL $ numerator rat))) (VarE $ mkName "Data.Ratio.%") (Just (LitE (IntegerL $ denominator rat))))
    
  toExp (JsonIdVar v) =
    VarE $ mkName v

  toExp (JsonBool b) =
    AppE (ConE $ mkName "Text.JSON.JSBool") (ConE $ mkName (if b then "True" else "False"))

  toExp (JsonCode exp) =
    AppE (VarE $ mkName "Text.JSON.Generic.toJSON") exp

-------
-- ToJsonOrId

-- class ToJsonOrId a where
--   toJsonOrId :: a -> JSValue

-------
-- Internal representation

data JsonValue =
  JsonNull
  | JsonString String
  | JsonNumber Bool Rational
  | JsonObject [(HashKey,JsonValue)] -- [(String,JsonValue)]
  | JsonArray [JsonValue]
  | JsonIdVar String
  | JsonBool Bool
  | JsonCode Exp

data HashKey =
  HashVarKey String
  | HashStringKey String

------
-- Grammar
-- jp = json parsec
-----

(=>>) :: Monad m => m a -> b -> m b
x =>> y = x >> return y


(>>>=) :: Monad m => m a -> (a -> b) -> m b
x >>>= y = x >>= return . y

type JsonParser = Parser JsonValue

data QQJsCode =
  QQjs JSValue
  | QQcode String

jpValue :: JsonParser
jpValue = do
  spaces
  res <- jpTrue <|> jpFalse <|> try jpIdVar <|> jpNull <|> jpString <|> jpObject <|> jpNumber  <|> jpArray <|> jpCode
  spaces
  return res

jpTrue :: JsonParser
jpTrue = jpBool "true" True

jpFalse :: JsonParser
jpFalse = jpBool "false" False

jpBool :: String -> Bool -> JsonParser
jpBool txt b = string txt =>> JsonBool b

jpCode :: JsonParser
jpCode = do
  string "<|"
  parseExp' >>>= JsonCode
  where
    parseExp' = do
      str <- untilString
      case (parseExp str) of
        Left l -> fail l
        Right r -> return r




jpIdVar :: JsonParser
jpIdVar = between (string "<<") (string ">>") symbol >>>= JsonIdVar


jpNull :: JsonParser
jpNull = do
  string "null" =>> JsonNull

jpString :: JsonParser 
jpString = between (char '"') (char '"') (option [""] $ many chars) >>= return . JsonString . concat -- do

jpNumber :: JsonParser 
jpNumber = do
  val <- float
  return $ JsonNumber False (toRational val)

jpObject :: JsonParser
jpObject = do
  list <- between (char '{') (char '}') (commaSep jpHash)
  return $ JsonObject $ list
  where
    jpHash :: CharParser () (HashKey,JsonValue) -- (String,JsonValue)
    jpHash = do
      spaces
      name <- varKey <|> symbolKey <|> quotedStringKey
      spaces
      char ':'
      spaces
      value <- jpValue
      spaces
      return (name,value)

symbolKey :: CharParser () HashKey
symbolKey = symbol >>>= HashStringKey

quotedStringKey :: CharParser () HashKey
quotedStringKey = quotedString >>>= HashStringKey

varKey :: CharParser () HashKey
varKey = do
  char '$'
  sym <- symbol
  return $ HashVarKey sym

jpArray :: CharParser () JsonValue
jpArray = between (char '[') (char ']') (commaSep jpValue) >>>= JsonArray

-------
-- helpers for parser/grammar

untilString :: Parser String
untilString = do
      n0 <- option "" $ many1 (noneOf "|")
      char '|'
      n1 <- option "" $ many1 (noneOf ">")
      char '>'
      if not $ null n1
        then do n2 <- untilString
                return $ concat [n0,n1,n2]
        else return $ concat [n0,n1]
    


float :: CharParser st Double 
float = do
  isMinus <- option ' ' (char '-')
  d <- many1 digit
  o <- option "" withDot
  e <- option "" withE
  return $ (read $ isMinus : d ++ o ++ e :: Double)

withE = do
  e <- char 'e' <|> char 'E'
  plusMinus <- option "" (string "+" <|> string "-")
  d <- many digit
  return $ e : plusMinus ++ d 

withDot = do
  o <- char '.'
  d <- many digit
  return $ o:d

quotedString :: CharParser () String
quotedString = between (char '"') (char '"') (option [""] $ many chars) >>>= concat

symbol :: CharParser () String
symbol = many1 (noneOf "\\ \":;><$")

commaSep p  = p `sepBy` (char ',')

chars :: CharParser () String
chars = do
   try (string "\\\"")
   <|> try (string "\\/")
   <|> try (string "\\\\")
   <|> try (string "\\b")
   <|> try (string "\\f")
   <|> try (string "\\n")
   <|> try (string "\\r")
   <|> try (string "\\t")
   <|> try (unicodeChars)
   <|> many1 (noneOf "\\\"")

unicodeChars :: CharParser () String
unicodeChars = do
  u <- string "\\u"
  d1 <- hexDigit
  d2 <- hexDigit
  d3 <- hexDigit
  d4 <- hexDigit
  return $ u ++ [d1] ++ [d2] ++ [d3] ++ [d4]
