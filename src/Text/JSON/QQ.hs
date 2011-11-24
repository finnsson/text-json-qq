{-# OPTIONS_GHC -XTemplateHaskell -XQuasiQuotes -XUndecidableInstances #-}

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

import Data.JSON.QQ

import Data.Ratio
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
-- import Text.Parsec.Perm

import Language.Haskell.Meta.Parse

jsonQQ :: QuasiQuoter
jsonQQ = QuasiQuoter { 
  quoteExp = jsonExp, 
  quotePat = \s -> error "No quotePat defined for jsonQQ",
  quoteType = \s -> error "No quoteType defined for jsonQQ",
  quoteDec = \s -> error "No quoteDec defined for jsonQQ"
}


jsonExp :: String -> ExpQ
jsonExp txt =
  case parsed' of 
    Left err -> error $ "Error in jsonExp: " ++ show err
    Right val -> return $ toExp val
  where
    parsed' = parsedJson txt

----
-- JSValue etc to ExpQ
---------
toExp :: JsonValue -> Exp
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
