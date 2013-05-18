{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)
import Control.Monad.IO.Class
import Text.CSV
import Data.List
import System.Directory
import System.Process
import Text.Printf


data FeedBot = FeedBot {
  botId :: Integer,
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
convertTokenToFeedBots = map convertTokenToFeedBot . filter (\x -> length x == 7)

convertTokenToFeedBot :: [String] -> FeedBot
convertTokenToFeedBot [n,se,p,c,us,ur,sp] = 
  FeedBot {
     botId = (read n) :: Integer,
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
    "ID",
    "IRC Server",
    "IRC Port",
    "IRC Channel",
    "IRC User",
    "Feed URL",
    "Span"
    ]) ++ "</tr>\n"

convertFeedBotToHtml :: FeedBot -> String
convertFeedBotToHtml FeedBot {
  botId=n,
  ircServer=is,
  ircPort=p,
  ircChannel=c,
  ircUser=iu,
  feedUrl=fu,
  feedSpan=fs
  } = concat $ map (\x -> "<td>" ++ x ++ "</td>") [(show n),is,(show p),c,iu,fu,(show fs)]

createBot :: String->Integer->String->String->String->Integer -> IO ()
createBot server port channel user url span = do
  bots <- parseFeedBotsFromFile "bots.csv"
  let bot = FeedBot {
            botId = (genericLength bots + 1),
            ircServer = server,
            ircPort = port,
            ircChannel = channel,
            ircUser = user,
            feedUrl = url,
            feedSpan = span
            }
  saveFeedBotsToFile "bots-new.csv" (bots ++ [bot])
  removeFile "bots.csv"
  renameFile "bots-new.csv" "bots.csv"

saveFeedBotsToFile :: FilePath -> [FeedBot] -> IO ()
saveFeedBotsToFile path bots =
  let csv = map convertFeedBotToList bots
      contents = printCSV csv
    in writeFile path contents
    
convertFeedBotToList :: FeedBot -> [String]
convertFeedBotToList bot =
  [
    show (botId bot),
    ircServer bot,
    show (ircPort bot),
    ircChannel bot,
    ircUser bot,
    feedUrl bot,
    show (feedSpan bot)
  ]

executeFeedBot :: (Integral n) => n -> IO ProcessHandle
executeFeedBot n = do
  bots <- parseFeedBotsFromFile "bots.csv"
  forkFeedBot (bots `genericIndex` (n-1))
          
forkFeedBot :: FeedBot -> IO ProcessHandle
forkFeedBot bot = 
  runCommand $ printf "runghc feedbot.hs --irc-server '%s' --port %d --channel '%s' --nick '%s' --realname '%s' --username '%s' --feed-url '%s' --feed-span %d" (ircServer bot) (ircPort bot) (ircChannel bot) user user user (feedUrl bot) (feedSpan bot)
  where
    user = ircUser bot
  
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

  post "/bots" $ do
    server <- param "server"
    port <- param "port"
    channel <- param "channel"
    user <- param "user"
    url <- param "url"
    span <- param "span"
    liftIO $ createBot server ((read port) :: Integer) channel user url ((read span) :: Integer)
    redirect "/bots"

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

  post "/bots/:id/execute" $ do
    id <- param "id"
    liftIO $ executeFeedBot (read id)
    redirect "/bots"
