{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell -XQuasiQuotes #-}
module Text.JSON.TestQQ where

import Text.JSON.QQ

-- for test
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework (defaultMain)

import Text.JSON -- (toJSObject)
import Text.JSON.Types

import Data.Ratio

-- import Data.Ratio

import Language.Haskell.TH 

main = defaultMain [tests]

tests = $testGroupGenerator

case_get_QQ_to_compile = do
  let actual = [$jsonQQ| {foo: "bar"} |]
      expected = JSObject $ toJSObject [("foo", JSString $ toJSString "bar")]
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
