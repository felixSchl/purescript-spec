module Test.Spec where

import Effect.Aff.AVar
import Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT, execWriterT, tell)
import Data.Array (intercalate)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as Console

data Group a
  = Describe String (Array (Group a))
  | It String a

derive instance functorGroup :: Functor Group

type Spec m a =
  WriterT (Array (Group a)) m Unit

class (Monad m, Applicative f) <= MonadSpec m f where
  it :: ∀ a. String -> a -> Spec m (f a)
  describe :: ∀ a. String -> Spec m (f a) -> Spec m (f a)
  beforeEach :: ∀ a b. f a -> Spec m (f (a -> b)) -> Spec m (f b)
  beforeAll :: ∀ a b. f a -> Spec m (f (a -> b)) -> Spec m (f b)

newtype Collector m a = Collector (m a)

derive newtype instance functorCollector :: Functor m => Functor (Collector m)
derive newtype instance applyCollector :: Apply m => Apply (Collector m)
derive newtype instance applicativeCollector :: Applicative m => Applicative (Collector m)
derive newtype instance monadCollector :: Monad m => Monad (Collector m)
derive newtype instance bindCollector :: Bind m => Bind (Collector m)
derive newtype instance monadAff :: MonadAff m => MonadAff (Collector m)

derive instance newtypeCollector :: Newtype (Collector m a) _

instance collectorMonadSpec
  :: (Monad m, MonadAff m)
  => MonadSpec (Collector m) Aff
  where
  it name body = tell [ It name $ pure body ]
  describe name spec = do
     groups <- lift $ execWriterT spec
     tell [ Describe name groups ]
  beforeEach setup spec = do
    groups <- lift $ execWriterT spec
    tell $ groups <#> map \ff -> do
        v <- setup
        f <- ff
        pure $ f v
  beforeAll setup spec = do
    lockV   <- liftAff $ AVar.new unit
    resultV <- liftAff AVar.empty
    beforeEach <@> spec $ do
      liftAff (AVar.tryTake lockV) >>= case _ of
        Nothing -> liftAff $ AVar.read resultV
        Just _  -> do
          v <- setup
          v <$ do
            liftAff $ AVar.put v resultV

collect
  :: ∀ m a
   . Monad m
  => Spec (Collector m) (m (m a))
  -> m (Array (Group (m a)))
collect spec = do
  groups <- unwrap $ execWriterT spec
  pure (map (map join) groups)

run
  :: ∀ m
   . MonadAff m
  => Spec (Collector m) (m (m Unit))
  -> m Unit
run spec = do
  groups <- collect spec
  traverse_ (go mempty) groups
  where
    go ctx (Describe name groups) =
      traverse_ (go (ctx <> [name])) groups
    go ctx (It name body) =
      let heading = intercalate " > " ctx <> " > it " <> name
       in do
        liftAff $ Console.log heading
        body
