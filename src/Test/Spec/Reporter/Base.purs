module Test.Spec.Reporter.Base (
  update,
  summarize,
  reporter,
  defaultReporter,
  onSummarize,
  onUpdate,
  BaseReporter
  ) where

import Prelude

import Data.String as   String
import Data.Array as    Array
import Data.Traversable (for_)
import Data.Array       ((:), reverse)
import Data.Foldable    (intercalate)

import Control.Monad.Eff              (Eff)
import Control.Monad.Eff.Console      (CONSOLE, log)
import Control.Monad.State            (StateT, evalStateT)
import Control.Monad.State as         State
import Control.Monad.Trans.Class      (lift)
import Control.Monad.Eff.Exception as Error

import Test.Spec as           S
import Test.Spec              (Group(), Result(..))
import Test.Spec.Runner.Event (Event)
import Test.Spec.Summary      as Summary
import Test.Spec.Summary      (Summary(..))
import Test.Spec.Console      (withAttrs)

-- TODO: move these somewhere central (Test.Spec.Console?)
red   = withAttrs [31]
green = withAttrs [32]
blue  = withAttrs [36]

type Update s m r = s -> Event -> m r
type Summarize s m = s -> Array (Group Result) -> m Unit

newtype BaseReporter s m = BaseReporter {
  state     :: s
, update    :: Update s m (BaseReporter s m)
, summarize :: Summarize s m
}

update :: ∀ s m. Event -> BaseReporter s m -> m (BaseReporter s m)
update e (BaseReporter { state: s, update: f }) = f s e

summarize :: ∀ s m. Array (Group Result) -> BaseReporter s m -> m Unit
summarize xs (BaseReporter { state, summarize: f }) = f state xs

onUpdate
  :: ∀ s m
   . (Monad m)
  => Update s m s
  -> BaseReporter s m
  -> BaseReporter s m
onUpdate update (BaseReporter s) = reporter s.state update s.summarize

onSummarize
  :: ∀ s m
   . Summarize s m
  -> BaseReporter s m
  -> BaseReporter s m
onSummarize summarize (BaseReporter s) = BaseReporter $ s { summarize = summarize }

reporter
  :: ∀ s m
   . (Monad m)
  => s
  -> Update s m s
  -> Summarize s m
  -> BaseReporter s m
reporter state update summarize = BaseReporter { state, update: go, summarize }
  where
  go s e = do
    s' <- update s e
    pure $ BaseReporter { state: s', update: go, summarize }

defaultReporter :: ∀ s e. s -> BaseReporter s (Eff (console :: CONSOLE | e))
defaultReporter s = reporter s defaultUpdate defaultSummary
  where
  defaultUpdate s _ = pure s
  defaultSummary  _ xs = do
    case Summary.summarize xs of
      (Count passed failed pending) -> do
        when (passed  > 0) $ green $ log $ show passed  <> " passing"
        when (pending > 0) $ blue  $ log $ show pending <> " pending"
        when (failed  > 0) $ red   $ log $ show failed  <> " failed"
    log ""
    printFailures xs

  printFailures
    :: Array (Group Result)
    -> Eff (console :: CONSOLE | e) Unit
  printFailures xs = void $ evalStateT (go [] xs) 0
    where
    go
      :: Array String
      -> Array (Group Result)
      -> StateT Int (Eff (console :: CONSOLE | e)) Unit
    go crumbs groups =
      for_ groups case _ of
        S.Describe n xs -> go (n:crumbs) xs
        S.It n (Failure err) ->
          let label = intercalate " " (reverse $ n:crumbs)
            in do
                State.modify (_ + 1)
                i <- State.get
                lift $ log $ show i <> ") " <> label
                lift $ red $ log $ indent 2 <> Error.message err
        _ -> pure unit

  -- TODO: move this somewhere central
  indent i = String.fromCharArray $ Array.replicate i ' '

