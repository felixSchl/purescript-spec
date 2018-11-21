module Test.Main where

import Prelude

import Effect (Effect)
import Control.Monad.Error.Class (throwError)
import Effect.Aff (launchAff_)
import Effect.Exception (error)
import Test.Spec (beforeAll, beforeEach, describe, it)
import Test.Spec as Spec

main :: Effect Unit
main = launchAff_ $ Spec.run $
  describe "foo" do
    beforeAll (pure 10) do
      beforeEach (pure 20) do
        it "bar" \a b -> do
          unless (a == 20) $ throwError (error $ "a /= 10: " <> show a)
          unless (b == 10) $ throwError (error $ "b /= 20: " <> show b)
