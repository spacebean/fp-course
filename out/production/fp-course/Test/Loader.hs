{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test.Loader
  ( test_Optional,
    test_List,
    test_Functor,
    test_Applicative,
    test_Monad,
    test_State,
    test_StateT,
    test_Validation,
    test_Extend,
    test_Comonad,
    test_Traversable,
    test_ListZipper,
    test_Parser,
    test_MoreParser,
    test_JsonParser,
    test_Cheque,
    test,
    allTests,
  )
where

import Data.String (fromString)
import Test.ApplicativeTest (test_Applicative)
import Test.ChequeTest (test_Cheque)
import Test.ComonadTest (test_Comonad)
import Test.ExtendTest (test_Extend)
import Test.Framework (TestTree, test, testGroup)
import Test.FunctorTest (test_Functor)
import Test.JsonParserTest (test_JsonParser)
import Test.ListTest (test_List)
import Test.ListZipperTest (test_ListZipper)
import Test.MonadTest (test_Monad)
import Test.MoreParserTest (test_MoreParser)
import Test.OptionalTest (test_Optional)
import Test.ParserTest (test_Parser)
import Test.StateTTest (test_StateT)
import Test.StateTest (test_State)
import Test.TraversableTest (test_Traversable)
import Test.ValidationTest (test_Validation)

allTests :: TestTree
allTests =
  testGroup
    "Tests"
    [ test_Optional,
      test_List,
      test_Functor,
      test_Applicative,
      test_Monad,
      test_State,
      test_StateT,
      test_Validation,
      test_Extend,
      test_Comonad,
      test_Traversable,
      test_ListZipper,
      test_Parser,
      test_MoreParser,
      test_JsonParser,
      test_Cheque
    ]
