module Test.Spec where

import Prelude
import Data.Newtype (class Newtype, unwrap)
import Data.Maybe (Maybe(..))
import Data.Array (intercalate)
import Data.Foldable (traverse_)
import Data.Monoid (mempty)
import Control.Monad.IO (IO)
import Control.Monad.IO.Class (class MonadIO, liftIO)
import Control.Monad.Aff.AVar
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console as Console
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT, execWriterT, tell)

data Group a
  = Describe String (Array (Group a))
  | It String a

derive instance functorGroup :: Functor Group

type Spec m a = WriterT (Array (Group a)) m Unit

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
derive newtype instance monadIO :: MonadIO m => MonadIO (Collector m)

derive instance newtypeCollector :: Newtype (Collector m a) _

instance collectorMonadSpec
  :: (Monad m, MonadIO m)
  => MonadSpec (Collector m) IO
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
    lockV   <- liftIO $ liftAff $ makeVar' unit
    resultV <- liftIO $ liftAff makeVar
    flip beforeEach spec do
      liftAff (tryTakeVar lockV) >>= case _ of
        Nothing -> liftAff $ peekVar resultV
        Just _  -> do
          v <- setup
          v <$ do
            liftAff $ putVar resultV v

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
   . MonadIO m
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
        liftIO $ liftAff $ Console.log heading
        body

