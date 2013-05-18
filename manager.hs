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
      "<a href='/bots/new'>Create New bots</a>",
      "<table>",
      pack $ convertFeedBotsToHtml contents,
      "</table>",
      "</body>",
      "</html>"]

  get "/bots/new" $ do
    html $ mconcat [
      "<html>",
      "<head>",
      "<title>Create a New IRC Bot</title>",
      "</head>",
      "<body>",
      "<h2>Create a New IRC Bot</h2>",
      "<form method='POST' action='/bots'>",
      "IRC Server: <input name='server' type='text'/><br/>",
      "IRC Port: <input name='port' type='text'/><br/>",
      "IRC Channel: <input name='channel' type='text'/><br/>",
      "IRC User: <input name='user' type='text'/><br/>",
      "Feed URL: <input name='url' type='text'/><br/>",
      "Span: <input name='span' type='text'/><br/>",
      "<input type='submit' value='create bot!'/><br/>",
      "</form>",
      "</body>",
      "</html>"
      ]
