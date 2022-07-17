{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics
import           Network.HTTP.Types
import           Text.Read                      ( readMaybe )
import           Web.Scotty

data Recipe = Recipe
  { waterVolume  :: Int
  , coffeeWeight :: Int
  }
  deriving (Show, Generic)

instance ToJSON Recipe

waterRoute :: ScottyM ()
waterRoute = get "/water/:desiredVolume" $ do
  desiredVolume <- param "desiredVolume"
  case readMaybe desiredVolume :: Maybe Int of
    Just desiredVolume ->
      let recipe = Recipe { waterVolume  = desiredVolume
                          , coffeeWeight = desiredVolume `div` 17
                          }
      in  json recipe
    Nothing -> do
      status status400
      text "invalid path, desired volume should be a number"

coffeeRoute :: ScottyM ()
coffeeRoute = get "/coffee/:desiredWeight" $ do
  desiredWeight <- param "desiredWeight"
  case readMaybe desiredWeight :: Maybe Int of
    Just desiredWeight ->
      let recipe = Recipe { waterVolume  = desiredWeight * 17
                          , coffeeWeight = desiredWeight
                          }
      in  json recipe
    Nothing -> do
      status status400
      text "invalid path, desired weight should be a number"

main = do
  putStrLn "starting coffee calculator"
  scotty 3000 $ do
    waterRoute
    coffeeRoute
