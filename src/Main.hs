{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Data.ByteString as BS
import           Data.Text.Encoding     (decodeUtf8)
import           Data.Text              (splitOn, Text)
import qualified Data.Text as T
import           Data.Char              (isSpace)
import           Data.Monoid            ( (<>) )
import           Network.StatsD.Datadog (MetricName(MetricName), StatsClient, withDogStatsD, defaultSettings, send, event, metric, MetricType(Counter, Gauge))
import           Data.String            (IsString, fromString)
import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever)
import           Data.IORef             (IORef, newIORef, atomicModifyIORef)
import           Control.Lens           (makeLenses, Lens', view, set)

instance IsString MetricName where
  fromString = MetricName . T.pack

data CounterHacks = CounterHacks {
    _hits :: Int
  , _misses :: Int
  , _demand_data_hits :: Int
  , _demand_data_misses :: Int
  , _l2_hits            :: Int
  , _l2_misses          :: Int
}

makeLenses ''CounterHacks

main :: IO ()
main = do
  counters <- newIORef $ CounterHacks 0 0 0 0 0 0
  withDogStatsD defaultSettings $ \client -> do
    send client $ event "program start" "the program was started"
    forever $ do
      raw <- BS.readFile "/proc/spl/kstat/zfs/arcstats"
      let coredata = filter (/= "") (drop 2 $ splitOn "\n" $ decodeUtf8 raw)
      mapM_ (sendRowToEkg client counters . parseRow) coredata
      threadDelay (1000 * 1000 * 10)

doCounter :: StatsClient -> MetricName -> Lens' CounterHacks Int -> IORef CounterHacks -> Int -> IO ()
doCounter client name fn ref value = do
  let
    modifier :: CounterHacks -> (CounterHacks, Maybe Int)
    modifier counters =
      if view fn counters == 0 then
        (set fn value counters, Nothing)
      else
        (set fn value counters, Just $ value - view fn counters)
  diff' <- atomicModifyIORef ref modifier
  case diff' of
    Just diff -> send client $ metric name Counter diff
    Nothing -> pure ()

sendRowToEkg :: StatsClient -> IORef CounterHacks -> (Text, Int) -> IO ()
sendRowToEkg client counters (key, value) =
  case key of
    "hits"               -> doCounter client "zfs.hits"               hits               counters value
    "misses"             -> doCounter client "zfs.misses"             misses             counters value
    "demand_data_hits"   -> doCounter client "zfs.demand_data_hits"   demand_data_hits   counters value
    "demand_data_misses" -> doCounter client "zfs.demand_data_misses" demand_data_misses counters value
    "l2_hits"            -> doCounter client "zfs.l2_hits"            l2_hits            counters value
    "l2_misses"          -> doCounter client "zfs.l2_misses"          l2_misses          counters value
    "l2_size"            -> send client $ metric "zfs.l2_size" Gauge value
    "c"                  -> send client $ metric "zfs.c"       Gauge value
    "size"               -> send client $ metric "zfs.size"    Gauge value
    _ ->
      if value == 0 then
        pure ()
      else
        print $ "unhandled " <> key <> " " <> (T.pack $ show value)

parseRow :: Text -> (Text, Int)
parseRow input =
  let
    [ key, _, value] = filter (/="") (T.split isSpace input)
  in
    (key, read (T.unpack value))
