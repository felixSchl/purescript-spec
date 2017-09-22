module Test.Main where

import Prelude
import Control.Monad.IO
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Aff.Console as Console
import Test.Spec

main :: Eff _ Unit
main = void $ launchAff do
  runIO $ run do
    describe "foo" do
      beforeAll (pure unit) do
        beforeEach (pure unit) do
          it "bar" \_ _ -> do
            liftAff $ Console.log "hey"
