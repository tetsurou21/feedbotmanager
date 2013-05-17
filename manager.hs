{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)
import Control.Monad.IO.Class
import Text.CSV
data FeedBot = FeedBot {
  ircServer :: String,
  ircPort :: Integer,
  ircChannel :: String,
  ircUser :: String,
  feedUrl :: String,
  feedSpan :: Integer
} deriving (Show)

parseFeedBotsFromFile :: String -> IO [FeedBot]
parseFeedBotsFromFile path = do
  csv <- parseCSVFromFile path
  return $
    case csv of
      Left _ -> []
      Right csv -> convertTokenToFeedBots csv
      
convertTokenToFeedBots :: [[String]] -> [FeedBot]
convertTokenToFeedBots = map convertTokenToFeedBot . filter (\x -> length x == 6)

convertTokenToFeedBot :: [String] -> FeedBot
convertTokenToFeedBot [se,p,c,us,ur,sp] = 
  FeedBot {
     ircServer = se,
     ircPort = (read p) :: Integer,
     ircChannel = c,
     ircUser = us,
     feedUrl = ur,
     feedSpan = (read sp) :: Integer
     }

convertTokenToFeedBot x =
  error $ "failed to parse: " ++ (show x)


main = scotty 3000 $ do
  get "/bots" $ do
    contents <- liftIO $ parseFeedBotsFromFile "bots.csv"
    html $ mconcat [
      "<div>",
      pack $ show contents,
      "</div>"
      ]
