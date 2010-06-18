{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell -XQuasiQuotes #-}

-- | This package expose the function @jsonQQ@ that compile time converts json code into a @Text.JSON.JSValue@.
--    @jsonQQ@ got the signature
--    
--    > jsonQQ :: QuasiQuoter
--    
--    and is used like
--    
--    > myCode = [$jsonQQ| {age: 23, name: "Pelle", likes: ["mac","Haskell"] } |]
--    
--    where it is important that
--    
--    * you got no space in @[$jsonQQ|@ and
--    
--    * no additional code after @|]@.
--    
--    The quasiquatation can also bind to variables like
--    
--    > myCode = [$jsonQQ | {age: <<age>>, name: <<name>>} |]
--    > where age = 34 :: Integer
--    >       name = "Pelle"
--    
--    where a function  @toJsonOrId@ (that is also exported by @Text.JSON.QQ@) will be called on @age@ and @name@ runtime.

module Text.JSON.QQ (
  jsonQQ,
  ToJsonOrId (..)
) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
-- import Language.Haskell.Meta.Parse

import Data.Data
import Data.Maybe

import Text.JSON
import Text.JSON.Generic

import Data.Ratio
-- import Parsec
import Text.ParserCombinators.Parsec
-- import qualified Data.ByteString.Lazy as L
-- import Codec.Binary.UTF8.String (decode)
import Text.ParserCombinators.Parsec.Error

jsonQQ :: QuasiQuoter
jsonQQ = QuasiQuoter jsonExp jsonPat


jsonExp :: String -> ExpQ
jsonExp txt =
  case parsed' of 
    Left err -> error $ "Error in jsonExp: " ++ show err
    Right val -> return $ toExp val -- test -- dataToExpQ undefined test -- [| test |] -- [| toJSObject [("a","b")] |] --
  where
    parsed' = parse jpValue "txt" txt
    -- test = JSObject $ toJSObject [("a",JSNull)]

jsonPat :: String -> PatQ
jsonPat s = undefined


----
-- JSValue etc to ExpQ
---------

class ToExp a where
  toExp :: a -> Exp

instance ToExp JsonValue where
  toExp (JsonString str) = 
    AppE (ConE $ mkName "Text.JSON.Types.JSString") (AppE (VarE $ mkName "Text.JSON.Types.toJSString") (LitE (StringL $ str)))

  toExp (JsonNull) = ConE $ mkName "Text.JSON.Types.JSNull"

  toExp (JsonObject objs) = 
    AppE (ConE $ mkName "Text.JSON.Types.JSObject") (AppE (VarE $ mkName "Text.JSON.Types.toJSObject") (ListE $ jsList ))
    where
      jsList :: [Exp] -- [(String,JSValue)]
      jsList = map objs2list (objs)
      objs2list :: (String,JsonValue) -> Exp
      objs2list (k,v) = TupE [LitE (StringL k), toExp v]

  toExp (JsonArray arr) =
    AppE (ConE $ mkName "Text.JSON.Types.JSArray") (ListE $ map toExp arr)

  toExp (JsonNumber b rat) =
    AppE (AppE (ConE $ mkName "Text.JSON.Types.JSRational") (ConE $ mkName (if b then "True" else "False"))) (InfixE (Just (LitE (IntegerL $ numerator rat))) (VarE $ mkName "Data.Ratio.%") (Just (LitE (IntegerL $ denominator rat))))
    
  toExp (JsonVar v) =
    AppE (VarE $ mkName "Text.JSON.QQ.toJsonOrId") (VarE $ mkName v)

  toExp (JsonBool b) =
    AppE (ConE $ mkName "Text.JSON.Types.JSBool") (ConE $ mkName (if b then "True" else "False"))

class ToJsonOrId a where
  toJsonOrId :: a -> JSValue

instance ToJsonOrId JSValue where
  toJsonOrId = id

instance ToJsonOrId String where
  toJsonOrId txt = Text.JSON.JSString $ Text.JSON.toJSString txt

instance ToJsonOrId Integer where
  toJsonOrId int = Text.JSON.JSRational True (int % 1)

-------
-- Internal representation

data JsonValue =
  JsonNull
  | JsonString String
  | JsonNumber Bool Rational
  | JsonObject [(String,JsonValue)]
  | JsonArray [JsonValue]
  | JsonVar String
  | JsonBool Bool

------
-- Grammar
-- jp = json parsec
-----

data QQJsCode =
  QQjs JSValue
  | QQcode String

jpValue :: CharParser st JsonValue
jpValue = do
  spaces
  res <- jpTrue <|> jpFalse <|> try jpVar <|> jpNull <|> jpString <|> jpObject <|> try jpNumber  <|> jpArray
  spaces
  return res

jpTrue :: CharParser st JsonValue
jpTrue = do
  string "true"
  return $ JsonBool True

jpFalse :: CharParser st JsonValue
jpFalse = do
  string "false"
  return $ JsonBool False

jpVar :: CharParser st JsonValue
jpVar = do
  string "<<"
  sym <- symbol
  string ">>"
  return $ JsonVar sym

jpNull :: CharParser st JsonValue
jpNull = do
  string "null"
  return $ JsonNull

jpString :: CharParser st JsonValue 
jpString = do
  char '"'
  sym <- try $ option [""] $ many chars
  char '"'
  return $ JsonString $ concat sym 

jpNumber :: CharParser st JsonValue 
jpNumber = do
  isMinus <- optionMaybe (char '-')
  val <- float
  return $ JsonNumber (not $ isJust isMinus) (toRational val)

jpObject :: CharParser st JsonValue
jpObject = do
  char '{'
  spaces
  list <- commaSep jpHash
  spaces
  char '}'
  return $ JsonObject $ list
  where
    jpHash :: CharParser st (String,JsonValue)
    jpHash = do
      spaces
      name <- symbol
      spaces
      char ':'
      spaces
      value <- jpValue
      spaces
      return (name,value)

jpArray :: CharParser st JsonValue
jpArray = do
  char '['
  spaces
  list <- commaSep jpValue
  spaces
  char ']'
  return $ JsonArray list

-------
-- helpers for parser/grammar

float :: CharParser st Double 
float = do
  d <- many1 digit
  o <- option "" withDot
  e <- option "" withE
  return $ (read $ d ++ o ++ e :: Double)

withE = do
  e <- char 'e' <|> char 'E'
  plusMinus <- option "" (string "+" <|> string "-")
  d <- many digit
  return $ e : plusMinus ++ d 

withDot = do
  o <- char '.'
  d <- many digit
  return $ o:d

symbol :: CharParser st String
symbol = many1 (noneOf "\\ \":;><") -- alphaNum -- should be replaced!

commaSep p  = p `sepBy` (char ',')

chars :: CharParser st String
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

unicodeChars :: CharParser st String
unicodeChars = do
  u <- string "\\u"
  d1 <- hexDigit
  d2 <- hexDigit
  d3 <- hexDigit
  d4 <- hexDigit
  return $ u ++ [d1] ++ [d2] ++ [d3] ++ [d4]
