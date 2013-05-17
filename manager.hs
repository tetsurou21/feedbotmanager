{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)
import Control.Monad.IO.Class
import Text.CSV
import Data.List


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


convertFeedBotsToHtml :: [FeedBot] -> String
convertFeedBotsToHtml xs =
  concat $  tableHeader : (map ((\x -> "<tr>" ++ x ++ "</tr>\n") . convertFeedBotToHtml) xs)

tableHeader :: String
tableHeader =
  "<tr>" ++ (concat $ map (\x -> "<th>" ++ x ++ "</th>") [
    "IRC Server",
    "IRC Port",
    "IRC Channel",
    "IRC User",
    "Feed URL",
    "Span"
    ]) ++ "</tr>\n"

convertFeedBotToHtml :: FeedBot -> String
convertFeedBotToHtml FeedBot {
  ircServer=is,
  ircPort=p,
  ircChannel=c,
  ircUser=iu,
  feedUrl=fu,
  feedSpan=fs
  } = concat $ map (\x -> "<td>" ++ x ++ "</td>") [is,(show p),c,iu,fu,(show fs)]

main = scotty 3000 $ do
  get "/bots" $ do
    contents <- liftIO $ parseFeedBotsFromFile "bots.csv"
    html $ mconcat [
      "<html>",
      "<head>",
      "<title>IRC Bots</title>",
      "</head>",
      "<body>",
      "<h2>IRC Bots</h2>",
      "<table>",
      pack $ convertFeedBotsToHtml contents,
      "</table>",
      "</body>",
      "</html>"]


