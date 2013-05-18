{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)
import Control.Monad.IO.Class
import Text.CSV
import Data.List
import System.Directory
import System.Posix.Process
import System.Posix.Types
import Text.Printf


data FeedBot = FeedBot {
  botId :: Integer,
  ircServer :: String,
  ircPort :: Integer,
  ircChannel :: String,
  ircUser :: String,
  feedUrl :: String,
  feedSpan :: Integer,
  processId :: ProcessID
} deriving (Show)

parseFeedBotsFromFile :: String -> IO [FeedBot]
parseFeedBotsFromFile path = do
  csv <- parseCSVFromFile path
  return $
    case csv of
      Left _ -> []
      Right csv -> convertTokenToFeedBots csv
      
convertTokenToFeedBots :: [[String]] -> [FeedBot]
convertTokenToFeedBots = map convertTokenToFeedBot . filter (\x -> length x == 8)

convertTokenToFeedBot :: [String] -> FeedBot
convertTokenToFeedBot [n,se,p,c,us,ur,sp,pid] = 
  FeedBot {
     botId = (read n) :: Integer,
     ircServer = se,
     ircPort = (read p) :: Integer,
     ircChannel = c,
     ircUser = us,
     feedUrl = ur,
     feedSpan = (read sp) :: Integer,
     processId = (read pid) :: ProcessID
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
    "Span",
    "PID"
    ]) ++ "</tr>\n"

convertFeedBotToHtml :: FeedBot -> String
convertFeedBotToHtml FeedBot {
  botId=n,
  ircServer=is,
  ircPort=p,
  ircChannel=c,
  ircUser=iu,
  feedUrl=fu,
  feedSpan=fs,
  processId=pid
  } = concat $ map (\x -> "<td>" ++ x ++ "</td>") [(show n),is,(show p),c,iu,fu,(show fs),(show pid)]

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
            feedSpan = span,
            processId = 0
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
    show (feedSpan bot),
    show (processId bot)
  ]

executeFeedBot :: (Integral n) => n -> IO ()
executeFeedBot n = do
  bots <- parseFeedBotsFromFile "bots.csv"
  putStrLn $ "bots:" ++ (show bots)
  pid <- forkFeedBot (bots `genericIndex` (n-1))
  let bot = bots `genericIndex` (n-1)
    in saveFeedBotsToFile "bots-new.csv" (
    replaceFeedBot bots (n-1) FeedBot {
       botId = (botId bot),
       ircServer = (ircServer bot),
       ircPort = (ircPort bot),
       ircChannel = (ircChannel bot),
       ircUser = (ircUser bot),
       feedUrl = (feedUrl bot),
       feedSpan = (feedSpan bot),
       processId = pid
      }
    )
  removeFile "bots.csv"
  renameFile "bots-new.csv" "bots.csv"

replaceFeedBot :: (Integral n) => [FeedBot] -> n -> FeedBot -> [FeedBot]

replaceFeedBot (x:xs) 0 newBot = newBot:xs
replaceFeedBot [] n newBot = []
replaceFeedBot (x:xs) n newBot = x:(replaceFeedBot xs (n-1) newBot)
  

forkFeedBot :: FeedBot -> IO ProcessID
forkFeedBot bot =
  forkProcess $ executeFile "./feedbot" True [
    "--irc-server", (ircServer bot),
    "--port", (show (ircPort bot)),
    "--channel", (ircChannel bot),
    "--nick", user,
    "--realname", user,
    "--username", user,
    "--feed-url", (feedUrl bot),
    "--feed-span", (show (feedSpan bot))
    ] Nothing
  where
    user = ircUser bot

terminateFeedBot :: (Integral n) => n -> IO ()
terminateFeedBot n = do
  bots <- parseFeedBotsFromFile "bots.csv"
  putStrLn $ "bots:" ++ (show bots)
  killFeedBot (bots `genericIndex` (n-1))
  let bot = (bots `genericIndex` (n-1))
  let newBots = replaceFeedBot bots (n-1) FeedBot {
            botId = (botId bot),
            ircServer = (ircServer bot),
            ircPort = (ircPort bot),
            ircChannel = (ircChannel bot),
            ircUser = (ircUser bot),
            feedUrl = (feedUrl bot),
            feedSpan = (feedSpan bot),
            processId = 0 :: ProcessID
        }
  saveFeedBotsToFile "bots-new.csv" newBots
  removeFile "bots.csv"
  renameFile "bots-new.csv" "bots.csv"  

killFeedBot :: FeedBot -> IO ()
killFeedBot bot = do
  pid <- forkProcess $ executeFile "kill" True [show (processId bot)] Nothing
  return ()


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

  post "/bots/:id/terminate" $ do
    id <- param "id"
    liftIO $ terminateFeedBot (read id)
    redirect "/bots"
