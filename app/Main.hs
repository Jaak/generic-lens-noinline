{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Lens.Combinators
import Control.Monad
import Control.Monad.Reader
import Data.Generics.Product.Any
import Data.Generics.Product.Typed
import Data.IORef
import GHC.Generics
import Prelude

type family HasReader es e m where
  HasReader '[] r m = MonadReader r m
  HasReader (x ': xs) e m = (HasType x e, HasReader xs e m)

data AppState = AppState
  {counter :: Int}
  deriving stock (Generic)

data AppEnv = AppEnv
  {stateRef :: IORef AppState}
  deriving stock (Generic)

type App a = ReaderT AppEnv IO a

asksAppState :: (MonadIO m, HasReader '[IORef AppState] r m) => (AppState -> a) -> m a
asksAppState getter = do
  stateRef <- view $ the @(IORef AppState)
  liftIO $ getter <$> readIORef stateRef
{-# NOINLINE asksAppState #-}

-- asksAppState :: (AppState -> a) -> App a
-- asksAppState getter = do
--   stateRef <- asks (.stateRef)
--   liftIO $ getter <$> readIORef stateRef
-- {-# NOINLINE asksAppState #-}

testApp :: Int -> App Int
testApp k =
  sum <$> forM [1 :: Int .. k] \_ -> do
    result <- asksAppState (.counter)
    stateRef <- asks (.stateRef)
    liftIO $ modifyIORef stateRef \s -> s {counter = result + 1 }
    pure result

runApp :: App a -> IO a
runApp m = do
  stateRef <- newIORef (AppState 0)
  let env = AppEnv {stateRef}
  runReaderT m env

main :: IO ()
main = do
  totals <- forM [1 :: Int .. 100] \_iter -> do
    r <- runApp (testApp 100000)
    pure $! r
  putStrLn $ "Total: " <> show (sum totals)
