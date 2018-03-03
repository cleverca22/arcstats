{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.ByteString as BS
import           Data.Text.Encoding     (decodeUtf8)
import           Data.Text              (splitOn, Text)
import qualified Data.Text as T
import           Data.Char              (isSpace)
import           Data.Reflection        (Given, give, given)
import           Data.Monoid            ( (<>) )
import           Network.StatsD.Datadog (MetricName(MetricName), StatsClient, withDogStatsD, defaultSettings, send, event, metric, MetricType(Counter, Gauge), ToMetricValue)
import           Data.String            (IsString, fromString)
import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever, when)
import           Data.IORef             (IORef, newIORef, atomicModifyIORef)
import           Control.Lens           (makeLenses, Lens', view, set)
import           Data.Time              ()
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           Data.Data              ()

type HasCounters = Given (IORef CounterHacks)

withCounters :: IORef CounterHacks -> (HasCounters => r) -> r
withCounters = give

getCounters :: HasCounters => IORef CounterHacks
getCounters = given

type HasClient = Given StatsClient

withClient :: StatsClient -> (HasClient => r) -> r
withClient = give

getClient :: HasClient => StatsClient
getClient = given

instance IsString MetricName where
  fromString = MetricName . T.pack

data CounterHacks = CounterHacks {
    _hits :: Int
  , _misses :: Int
  , _demand_data_hits :: Int
  , _demand_data_misses :: Int
  , _l2_hits            :: Int
  , _l2_misses          :: Int
  , _time :: Double
} deriving (Show)

makeLenses ''CounterHacks

main :: IO ()
main = do
  counters <- newIORef $ CounterHacks 0 0 0 0 0 0 0
  withDogStatsD defaultSettings $ \client -> do
    send client $ event "program start" "the program was started"
    withCounters counters $ withClient client $ forever $ do
      raw <- BS.readFile "/proc/spl/kstat/zfs/arcstats"
      let coredata = filter (/= "") (drop 2 $ splitOn "\n" $ decodeUtf8 raw)
      mapM_ (sendRowToEkg . parseRow) coredata
      now <- getPOSIXTime
      doCounter "test.time" time (realToFrac now)
      send client $ metric "test.fixed" Counter (1 :: Int)
      threadDelay (1000 * 1000 * 10)

doCounter :: forall a. (Real a, ToMetricValue a, HasCounters, HasClient) => MetricName -> Lens' CounterHacks a -> a -> IO ()
doCounter name fn value = do
  let
    modifier :: CounterHacks -> (CounterHacks, Maybe a)
    modifier counters =
      if view fn counters == 0 then
        (set fn value counters, Nothing)
      else
        (set fn value counters, Just $ value - view fn counters)
  diff' <- atomicModifyIORef getCounters modifier
  case diff' of
    Just diff -> send getClient $ metric name Counter diff
    Nothing -> pure ()

sendRowToEkg :: HasClient => HasCounters => (Text, Int) -> IO ()
sendRowToEkg (key, value) =
  case key of
    "hits"               -> doCounter "zfs.hits"               hits               value
    "misses"             -> doCounter "zfs.misses"             misses             value
    "demand_data_hits"   -> doCounter "zfs.demand_data_hits"   demand_data_hits   value
    "demand_data_misses" -> doCounter "zfs.demand_data_misses" demand_data_misses value
    "l2_hits"            -> doCounter "zfs.l2_hits"            l2_hits            value
    "l2_misses"          -> doCounter "zfs.l2_misses"          l2_misses          value
    "l2_size"            -> send getClient $ metric "zfs.l2_size" Gauge value
    "c"                  -> send getClient $ metric "zfs.c"       Gauge value
    "size"               -> send getClient $ metric "zfs.size"    Gauge value
    _ ->
      when (value /= 0) $ print $ "unhandled " <> key <> " " <> (T.pack $ show value)

parseRow :: Text -> (Text, Int)
parseRow input =
  let
    [ key, _, value] = filter (/="") (T.split isSpace input)
  in
    (key, read (T.unpack value))
