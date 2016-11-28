module Test.Spec.Reporter.Dot (dotReporter) where

import Prelude

import Control.Monad.Eff         (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Spec.Reporter.Base   (BaseReporter, defaultReporter, onUpdate)
import Test.Spec.Console         (write) as Console
import Test.Spec.Color           (colored)
import Test.Spec.Color as        Color
import Test.Spec.Runner.Event as Event
import Test.Spec.Reporter.Speed as Speed

type DotReporterState = Int
type DotReporterConfig = { slow :: Int, width :: Int }
type DotReporter r = BaseReporter DotReporterConfig DotReporterState r

dotReporter
  :: DotReporterConfig
  -> ∀ e. DotReporter (Eff (console :: CONSOLE | e))
dotReporter config
  = defaultReporter config (-1)
      # onUpdate update

 where
  update { slow, width } n = case _ of
    Event.Pass  _ ms ->
      let col = Speed.toColor' slow ms
       in wrap $ Console.write (colored col ".")
    Event.Fail  _ _ -> wrap $ Console.write (colored Color.Fail    "!")
    Event.Pending _ -> wrap $ Console.write (colored Color.Pass    ",")
    Event.End       -> n <$ Console.write "\n"
    _               -> pure n

    where
    wrap action =
      let n' = n + 1
       in n' <$ do
            when (n' `mod` width == 0) (Console.write "\n")
            action

