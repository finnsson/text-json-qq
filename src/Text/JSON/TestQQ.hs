{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell -XQuasiQuotes #-}
module Text.JSON.TestQQ where

import Text.JSON.QQ

-- for test
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework (defaultMain)

import Text.JSON
import Text.JSON.Types

import Data.Ratio

-- import Data.Ratio

import Language.Haskell.TH 

main = defaultMain [tests]

tests = $testGroupGenerator

case_get_QQ_to_compile = do
  let actual = [$jsonQQ| {foo: "ba r.\".\\.r\n"} |]
      expected = JSObject $ toJSObject [("foo", JSString $ toJSString "ba r.\\\".\\\\.r\\n")]
  expected @=? actual

case_arrays = do
  let actual = [$jsonQQ| [null,{foo: 42}] |]
      expected = JSArray [JSNull, JSObject $ toJSObject [("foo", JSRational True (42 % 1))] ]
  expected @=? actual

case_code = do
  let actual = [$jsonQQ| [null,{foo: <<x>>}] |]
      expected = JSArray [JSNull, JSObject $ toJSObject [("foo", JSRational True (42 % 1))] ]
      x = 42 :: Integer
  expected @=? actual

case_true = do
  let actual = [$jsonQQ| [true,false,null] |]
      expected = JSArray [JSBool True, JSBool False, JSNull]
  expected @=? actual

case_multiline = do
  let actual =
        [$jsonQQ|
          [{user: "Pelle"},
           {user: "Arne"}]
         |]
      expected = JSArray [JSObject $ toJSObject [("user", JSString $ toJSString "Pelle")], JSObject $ toJSObject [ ("user", JSString $ toJSString "Arne")] ]
  expected @=? actual
