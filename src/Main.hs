module Main (main) where

import BasicPrelude hiding (empty, lookup)
import qualified Control.Foldl as Fold
import Data.Aeson
import qualified Data.Map.Lazy as Map
import Turtle

-- TODO: Use Lens, which should be a nicer translation of jq.

-- TODO: Figure out how to avoid creating the "response" types. Should just be
-- able to manipulate an abstract JSON object

-- TODO: Nicer formatting

-- TODO: README explaining how to install and run.

-- TODO: Show which regions instances are in.

-- TODO: Filter for running.


data RegionsResponse = Regions { getRegions :: [Region] } deriving (Show)

instance FromJSON RegionsResponse where
  parseJSON (Object v) = Regions <$> v .: "Regions"
  parseJSON _ = mzero


data Region = Region { regionName :: Text
                     , endpoint :: Text } deriving (Eq, Show)


instance FromJSON Region where

  parseJSON (Object v) = Region <$> v .: "RegionName" <*> v .: "Endpoint"
  parseJSON _ = mzero



data InstanceResponse = Instances { getInstances :: [Instance] } deriving (Show)

instance FromJSON InstanceResponse where
  parseJSON (Object v) = do
    reservations <- v .: "Reservations"
    Instances <$> concat <$> mapM (.: "Instances") reservations
  parseJSON _ = mzero


data Instance = Instance { state :: Text
                         , keyName :: Text
                         } deriving (Eq, Show)

instance FromJSON Instance where
  parseJSON (Object v) = do
    state' <- v .: "State"
    Instance <$> state' .: "Name"
             <*> v .: "KeyName"
  parseJSON _ = mzero


decodeJSONText :: FromJSON a => Text -> Maybe a
decodeJSONText = decodeStrict . encodeUtf8


describeRegions :: Shell (Maybe [Region])
describeRegions =
  parseRegionsResponse <$> fold (inshell "aws ec2 --region eu-west-1 describe-regions" empty) Fold.mconcat
  where
    parseRegionsResponse = fmap getRegions . decodeJSONText


describeInstances :: Region -> Shell (Maybe [Instance])
describeInstances region =
  parseInstancesResponse <$> fold (inshell ("aws ec2 --region " ++ regionName region ++ " describe-instances") empty) Fold.mconcat
  where
    parseInstancesResponse = fmap getInstances . decodeJSONText


summarizeInstances :: [(Region, [Instance])] -> Map.Map Text Int
summarizeInstances instanceData =
  frequency keys
  where
    frequency = foldr (Map.alter increment) Map.empty

    increment Nothing = Just 1
    increment (Just i) = Just (i + 1)

    keys = map keyName $ concat [i | (_, i) <- instanceData]



main :: IO ()
main = sh $ do
  regions <- fromMaybe [] <$> describeRegions
  instances <- catMaybes <$> mapM describeInstances' regions
  liftIO $ print (summarizeInstances instances)

  where
    describeInstances' :: Region -> Shell (Maybe (Region, [Instance]))
    describeInstances' region = do
      instances <- describeInstances region
      return $ (,) region <$> instances
