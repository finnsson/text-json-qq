{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell -XQuasiQuotes #-}
module Text.JSON.QQ (
  jsonQQ,
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
    test = JSObject $ toJSObject [("a",JSNull)]

jsonPat :: String -> PatQ
jsonPat s = undefined


----
-- JSValue etc to ExpQ
---------

class ToExp a where
  toExp :: a -> Exp

instance ToExp JSValue where
  toExp (JSString str) = 
    AppE (ConE $ mkName "Text.JSON.Types.JSString") (AppE (VarE $ mkName "Text.JSON.Types.toJSString") (LitE (StringL $ fromJSString str)))

  toExp (JSNull) = ConE $ mkName "Text.JSON.Types.JSNull"

  toExp (JSObject objs) = 
    AppE (ConE $ mkName "Text.JSON.Types.JSObject") (AppE (VarE $ mkName "Text.JSON.Types.toJSObject") (ListE $ jsList ))
    where
      jsList :: [Exp] -- [(String,JSValue)]
      jsList = map objs2list (fromJSObject objs)
      objs2list :: (String,JSValue) -> Exp
      objs2list (k,v) = TupE [LitE (StringL k), toExp v]

  toExp (JSArray arr) =
    AppE (ConE $ mkName "Text.JSON.Types.JSArray") (ListE $ map toExp arr)

  toExp (JSRational b rat) =
    AppE (AppE (ConE $ mkName "Text.JSON.Types.JSRational") (ConE $ mkName "True")) (InfixE (Just (LitE (IntegerL $ numerator rat))) (VarE $ mkName "Data.Ratio.%") (Just (LitE (IntegerL $ denominator rat))))
    

------
-- Grammar
-- jp = json parsec
-----



jpValue :: CharParser st JSValue
jpValue = do
  spaces
  res <- jpNull <|> jpString <|> jpNumber <|> jpObject <|> jpArray
  spaces
  return res

jpNull :: CharParser st JSValue
jpNull = do
  string "null"
  return $ JSNull

jpString :: CharParser st JSValue 
jpString = do
  char '"'
  sym <- symbol
  char '"'
  return $ JSString $ toJSString sym 

jpNumber :: CharParser st JSValue 
jpNumber = do
  num <- many1 digit
  return $ JSRational True ((read num :: Integer) % 1)

jpObject :: CharParser st JSValue
jpObject = do
  char '{'
  spaces
  list <- commaSep jpHash
  spaces
  char '}'
  return $ JSObject $ toJSObject list
  where
    jpHash :: CharParser st (String,JSValue)
    jpHash = do
      spaces
      name <- symbol
      spaces
      char ':'
      spaces
      value <- jpValue
      spaces
      return (name,value)

jpArray :: CharParser st JSValue
jpArray = do
  char '['
  spaces
  list <- commaSep jpValue
  spaces
  char ']'
  return $ JSArray list

-------
-- helpers for parser/grammar

symbol :: CharParser st String
symbol = many1 (noneOf "\\ \":;") -- alphaNum -- should be replaced!

commaSep p  = p `sepBy` (char ',')
