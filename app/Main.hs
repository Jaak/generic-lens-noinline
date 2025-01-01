{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Lens.Combinators
import Control.Lens.TH ()
import Control.Monad
import Control.Monad.Reader
import Criterion.Measurement.Types (Measured (..), whnfIO, rescale)
import Data.Generics.Labels ()
import Data.Generics.Product.Any
import Data.IORef
import GHC.Generics
import GHC.Int (Int64)
import Prelude
import qualified Criterion.Measurement as Measurement
import System.Environment
import System.Exit
import System.IO
import Text.Read (readMaybe)

data AppState = AppState
  { counter :: Int}

data AppEnv = AppEnv
  { dummy1 :: Maybe Int
  , dummy2 :: Maybe Int
  , dummy3 :: Maybe Int
  , dummy4 :: Maybe Int
  , dummy5 :: Maybe Int
  , dummy6 :: Maybe Int
  , dummy7 :: Maybe Int
  , dummy8 :: Maybe Int
  , dummy9 :: Maybe Int
  , dummy10 :: Maybe Int
  , stateRef :: IORef AppState}
  deriving stock (Generic)

emptyAppEnv :: IORef AppState -> AppEnv
emptyAppEnv stateRef = AppEnv {
  dummy1 = Nothing,
  dummy2 = Nothing,
  dummy3 = Nothing,
  dummy4 = Nothing,
  dummy5 = Nothing,
  dummy6 = Nothing,
  dummy7 = Nothing,
  dummy8 = Nothing,
  dummy9 = Nothing,
  dummy10 = Nothing,
  stateRef}

$(makeLensesFor [("stateRef", "stateRefLens")] ''AppEnv)

type App a = ReaderT AppEnv IO a

asksAppStateThe :: (AppState -> a) -> App a
asksAppStateThe getter = do
  stateRef <- view $ the @(IORef AppState)
  liftIO $ getter <$> readIORef stateRef
{-# NOINLINE asksAppStateThe #-}

asksAppStateLabel :: (AppState -> a) -> App a
asksAppStateLabel getter = do
  stateRef <- view #stateRef
  liftIO $ getter <$> readIORef stateRef
{-# NOINLINE asksAppStateLabel #-}

asksAppStateTHLens :: (AppState -> a) -> App a
asksAppStateTHLens getter = do
  stateRef <- view stateRefLens
  liftIO $ getter <$> readIORef stateRef
{-# NOINLINE asksAppStateTHLens #-}

asksAppStateAsks :: (AppState -> a) -> App a
asksAppStateAsks getter = do
  stateRef <- asks (.stateRef)
  liftIO $ getter <$> readIORef stateRef
{-# NOINLINE asksAppStateAsks #-}

data WhatToBenchmark
  = BenchmarkThe
  | BenchmarkLabel
  | BenchmarkTHLens
  | BenchmarkAsks
  deriving stock (Enum)

asksAppState :: WhatToBenchmark -> (AppState -> a) -> App a
asksAppState = \case
  BenchmarkThe -> asksAppStateThe
  BenchmarkLabel -> asksAppStateLabel
  BenchmarkTHLens -> asksAppStateTHLens
  BenchmarkAsks -> asksAppStateAsks
{-# INLINE asksAppState #-}

runTestApp :: WhatToBenchmark -> Int -> IO Int
runTestApp what k = runApp do
  sum <$> forM [1 :: Int .. k] \_ -> do
    result <- asksAppState what (.counter)
    stateRef <- asks (.stateRef)
    liftIO $ modifyIORef stateRef \s -> s {counter = result + 1 }
    pure result
{-# INLINE runTestApp #-}

runApp :: App a -> IO a
runApp m = do
  stateRef <- newIORef (AppState 0)
  runReaderT m (emptyAppEnv stateRef)
{-# INLINE runApp #-}

main :: IO ()
main = do
  Measurement.initializeTime
  args <- getArgs
  (arg1, arg2) <- do
    case args of
      [arg1, arg2] -> pure (arg1, arg2)
      _ -> err
  what <- toEnum <$> do
    case readMaybe arg1 of
      Just (n :: Int) | 0 <= n, n <= 3 -> pure n
      _ -> err
  iterations <- do
    case readMaybe arg2 of
      Just (k :: Int64) | k > 0 -> pure k
      _ -> err
  (meas, _) <- flip Measurement.measure iterations do
    whnfIO (runTestApp what 100000)
  let measRescaled = rescale meas
  putStrLn $ "Avg. Time: " <> show measRescaled.measTime <> "s"

err :: IO a
err = do
  hPutStrLn stderr $
    unlines [
      "Usage: ./bench-the <0|1|2|3> <k>",
      " k - number of iterations",
      " 0 - benchmark using 'asksAppStateThe'",
      " 1 - benchmark using 'asksAppStateLabel'",
      " 2 - benchmark using 'asksAppStateTHLens'",
      " 3 - benchmark using 'asksAppStateAsks'"
    ]
  exitFailure