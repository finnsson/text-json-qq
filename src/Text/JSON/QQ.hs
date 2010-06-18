{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell -XQuasiQuotes #-}
module Text.JSON.QQ (
  jsonQQ,
  ToJsonOrId (..)
) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
-- import Language.Haskell.Meta.Parse

import Data.Data

import Text.JSON
import Text.JSON.Generic

import Data.Ratio
-- import Parsec
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy as L
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

  toExp (JsonNumber rat) =
    AppE (AppE (ConE $ mkName "Text.JSON.Types.JSRational") (ConE $ mkName "True")) (InfixE (Just (LitE (IntegerL $ rat))) (VarE $ mkName "Data.Ratio.%") (Just (LitE (IntegerL $ 1))))
    
  toExp (JsonVar v) =
    AppE (VarE $ mkName "Text.JSON.QQ.toJsonOrId") (VarE $ mkName v)

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
  | JsonNumber Integer
  | JsonObject [(String,JsonValue)]
  | JsonArray [JsonValue]
  | JsonVar String

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
  res <- try jpVar <|> jpNull <|> jpString <|> jpNumber <|> jpObject <|> jpArray
  spaces
  return res

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
  sym <- symbol
  char '"'
  return $ JsonString sym 

jpNumber :: CharParser st JsonValue 
jpNumber = do
  num <- many1 digit
  return $ JsonNumber (read num :: Integer)

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

symbol :: CharParser st String
symbol = many1 (noneOf "\\ \":;><") -- alphaNum -- should be replaced!

commaSep p  = p `sepBy` (char ',')
