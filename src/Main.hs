{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Prelude                hiding (reads)
import qualified Data.ByteString as BS
import           Data.Text.Encoding     (decodeUtf8)
import           Data.Text              (splitOn, Text)
import qualified Data.Text as T
import           Data.Char              (isSpace)
import           Data.Reflection        (Given, give, given)
import           Data.Monoid            ( (<>) )
import           Network.StatsD.Datadog (MetricName(MetricName), StatsClient, withDogStatsD, defaultSettings, send, event, metric, MetricType(Counter, Gauge), ToMetricValue, tag, tags)
import           Data.String            (IsString, fromString)
import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever, when)
import           Data.IORef             (IORef, newIORef, atomicModifyIORef)
import           Control.Lens           (makeLenses, Lens', view, set)
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           System.Directory       (doesPathExist)
import qualified Data.HashMap.Strict as HM
import           Data.Default           (Default(def))
import           Data.Maybe             (fromMaybe)

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

data IoStats = IoStats {
    _major        :: Int
  , _minor        :: Int
  , _name         :: Text
  , _reads        :: Int
  , _read_merged  :: Int
  , _read_sector  :: Int
  , _mili_reading :: Int
  , _writes       :: Int
  , _writes_merged:: Int
  , _write_sector :: Int
  , _mili_writing :: Int
  , _io_pending   :: Int
  , _mili         :: Int
  , _weighted_mili:: Int
} deriving (Show)

makeLenses ''IoStats

type IoStatCounters = IORef (HM.HashMap Text IoStats)
type HasIoCounters = Given IoStatCounters
withIoCounters :: IoStatCounters -> (HasIoCounters => r) -> r
withIoCounters = give
getIoCounters :: HasIoCounters => IoStatCounters
getIoCounters = given

instance Default IoStats where
  def = IoStats 0 0 "name" 0 0 0 0 0 0 0 0 0 0 0
instance Default CounterHacks where
  def = CounterHacks 0 0 0 0 0 0 0

main :: IO ()
main = do
  let
    diskStats :: FilePath
    diskStats = "/proc/diskstats"
    arcStats :: FilePath
    arcStats = "/proc/spl/kstat/zfs/arcstats"
  counters <- newIORef def
  iocounters <- newIORef HM.empty
  zfsSupport <- doesPathExist arcStats
  ioSupport <- doesPathExist diskStats
  withDogStatsD defaultSettings $ \client -> do
    send client $ event "program start" "the program was started"
    withIoCounters iocounters $ withCounters counters $ withClient client $ forever $ do
      when zfsSupport $ do
        raw <- BS.readFile arcStats
        let coredata = filter (/= "") (drop 2 $ splitOn "\n" $ decodeUtf8 raw)
        mapM_ (sendRowToEkg . parseRow) coredata
      when ioSupport $ do
        raw <- BS.readFile diskStats
        let
          coredata = filter (/= "") (splitOn "\n" $ decodeUtf8 raw)
        mapM_ (sendIoRowToEkg . parseIoRow) coredata
      now <- getPOSIXTime
      doCounter "test.time" time (realToFrac now)
      send client $ metric "test.fixed" Counter (1 :: Int)
      threadDelay (1000 * 1000 * 10)

doCounter :: forall a. (Real a, ToMetricValue a, HasCounters, HasClient) => MetricName -> Lens' CounterHacks a -> a -> IO ()
doCounter name' fn value = do
  let
    modifier :: CounterHacks -> (CounterHacks, Maybe a)
    modifier counters =
      if view fn counters == 0 then
        (set fn value counters, Nothing)
      else
        (set fn value counters, Just $ value - view fn counters)
  diff' <- atomicModifyIORef getCounters modifier
  case diff' of
    Just diff -> send getClient $ metric name' Counter diff
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
      pure ()
      --when (value /= 0) $ print $ "unhandled " <> key <> " " <> (T.pack $ show value)

doIoCounter :: (HasIoCounters, HasClient) => IoStats -> MetricName -> Lens' IoStats Int -> IO ()
doIoCounter current name' fn = do
  let
    device = _name current
    value = view fn current
    modifier :: HM.HashMap Text IoStats -> (HM.HashMap Text IoStats, Maybe Int)
    modifier hm =
      let
        v :: IoStats
        v = fromMaybe def $ HM.lookup device hm
        v2 :: Int
        v2 = view fn v
        change :: Int
        change = value - v2
        change' :: Maybe Int
        change' = if v2 == 0 then Nothing else Just change
        v3 = set fn value v
        hm' = HM.insert device v3 hm
      in (hm', change')
  diff' <- atomicModifyIORef getIoCounters modifier
  case diff' of
    Just diff -> send getClient $ (set tags) [ tag "dev" device ] $ metric name' Counter diff
    Nothing -> pure ()

sendIoRowToEkg :: HasIoCounters => HasClient => IoStats -> IO ()
sendIoRowToEkg stats = do
  let
    tags' = [ tag "dev" $ _name stats ]
    f = doIoCounter stats
  send getClient $ (set tags) tags' $ metric "iostat.pending" Gauge (_io_pending stats)
  f "iostat.reads" reads
  f "iostat.writes" writes

  f "iostat.reads_merged" read_merged
  f "iostat.writes_merged" writes_merged

  f "iostat.reads_sectors" read_sector
  f "iostat.writes_sectors" write_sector

  f "iostat.write_mili" mili_writing
  f "iostat.read_mili" mili_reading

  f "iostat.weighted_mili" weighted_mili
  f "iostat.mili" mili

parseRow :: Text -> (Text, Int)
parseRow input =
  let
    [ key, _, value] = filter (/="") (T.split isSpace input)
  in
    (key, read (T.unpack value))

parseIoRow :: Text -> IoStats
parseIoRow input =
  let
    readNum :: Text -> Int
    readNum = read . T.unpack
    parts = filter (/="") (T.split isSpace input)
    [ _major, _minor ] = map readNum $ take 2 parts
    [ _name ] = take 1 $ drop 2 parts
    [ _reads, _read_merged, _read_sector, _mili_reading, _writes, _writes_merged, _write_sector, _mili_writing, _io_pending, _mili, _weighted_mili ] = map readNum $ drop 3 parts
  in IoStats{..}
