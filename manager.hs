{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Data.Monoid (mconcat)
import Data.Text.Lazy (pack, Text)
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
  } = concat $ map (\x -> "<td>" ++ x ++ "</td>") [(botDetailLink n),is,(show p),c,iu,fu,(show fs),(show pid)]

botDetailLink :: Integer -> String
botDetailLink n =
  printf "<a href='/bots/%d'>%d</a>" n n

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

updateFeedBot :: FeedBot -> IO ()
updateFeedBot bot = do
  bots <- parseFeedBotsFromFile "bots.csv"
  saveFeedBotsToFile "bots-new.csv" $ replaceFeedBot bots (botId bot - 1) bot
  removeFile "bots.csv"
  renameFile "bots-new.csv" "bots.csv"

deleteFeedBot :: (Integral n) => n -> IO ()
deleteFeedBot n = do
  bots <- parseFeedBotsFromFile "bots.csv"
  saveFeedBotsToFile "bots-new.csv" $ updateFeedBotsId $ dropElem bots (n-1)
  removeFile "bots.csv"
  renameFile "bots-new.csv" "bots.csv"  

updateFeedBotsId :: [FeedBot] -> [FeedBot]
updateFeedBotsId bots =
  zipWith (\bot n -> FeedBot{
              botId = n,
              ircServer = ircServer bot,
              ircPort = ircPort bot,
              ircChannel = ircChannel bot,
              ircUser = ircUser bot,
              feedUrl = feedUrl bot,
              feedSpan = feedSpan bot,
              processId = processId bot
              }
          ) bots [1..]
                                 
show' :: (Show a) => a -> Text
show' a = pack $ show a         

dropElem :: (Integral n) => [a] -> n -> [a]
dropElem [] n = []
dropElem (_:xs) 0 = xs
dropElem (x:xs) n = x:(dropElem xs (n-1))

  
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
      "<a href='/bots/new'>Add</a>",
      "<table>",
      pack $ convertFeedBotsToHtml contents,
      "</table>",
      "</body>",
      "</html>"]

  -- botの詳細ページ
  get "/bots/:id" $ do
    id <- param "id"
    bots <- liftIO $ parseFeedBotsFromFile "bots.csv"
    let bot = bots `genericIndex` ((id-1) :: Integer)
    html $ mconcat [
      "<html>",
      "<head>",
      "<title>IRC Bot #", show' $ botId bot, "</title>",
      "</head>",
      "<body>",
      "<h2>IRC Bot #", show' $ botId bot, "</h2>",
      "ID: ", show' $ botId bot, "<br/>",
      "IRC Server: ", pack $ ircServer bot, "<br/>",
      "IRC Port: ", show' $ ircPort bot, "<br/>",
      "IRC Channel: ", pack $ ircChannel bot, "<br/>",
      "IRC User: ", pack $ ircUser bot, "<br/>",
      "Feed URL: ", pack $ feedUrl bot, "<br/>",
      "Feed Span: ", show' $ feedSpan bot, "<br/>",
      "Process ID: ", show' $ processId bot, "<br/>",
      "<a href='/bots/", show' $ botId bot, "/edit'>edit</a><br/>",
      "<a href='/bots'>Back</a><br/>",
      "<form method='POST' action='/bots/", show' id, "/execute'>",
      "<input type='submit' value='execute!'/>",
      "</form>",
      "<form method='POST' action='/bots/", show' id, "/terminate'>",
      "<input type='submit' value='terminate!'/>",
      "</form>",
      "<form method='POST' action='/bots/", show' id, "/delete'>",
      "<input type='submit' value='delete!'/>",
      "</form>",
      "</body>",
      "</html>"]

  -- 編集ページ 
  get "/bots/:id/edit" $ do
    id <- param "id"
    bots <- liftIO $ parseFeedBotsFromFile "bots.csv"
    let n = (read id :: Integer) - 1
    let bot = bots `genericIndex` n
    html $ mconcat [
      "<html>",
      "<head>",
      "<title>IRC Bot #", show' $ botId bot, "</title>",
      "</head>",
      "<body>",
      "<h2>IRC Bot #", show' $ botId bot, "</h2>",
      "<form method='POST' action='/bots/", show' $ botId bot, "/update'>",
      "IRC Server: <input name='server' type='text' value='", pack $ ircServer bot, "'/><br/>",
      "IRC Port: <input name='port' type='text' value='", show' $ ircPort bot, "'/><br/>",
      "IRC Channel: <input name='channel' type='text' value='", pack $ ircChannel bot, "'/><br/>",
      "IRC User: <input name='user' type='text' value='", pack $ ircUser bot, "'/><br/>",
      "Feed URL: <input name='url' type='text' value='", pack $ feedUrl bot, "'/><br/>",
      "Feed Span: <input name='span' type='text' value='", show' $ feedSpan bot, "'/><br/>",
      "<input type='submit' value='update!'/>",
      "<a href='/bots/", pack id, "'>Cancel</a>",
      "</form>",
      "</body>",
      "</html>"]

  post "/bots/:id/update" $ do
    idStr <- param "id"
    server <- param "server"
    portStr <- param "port"
    channel <- param "channel"
    user <- param "user"
    url <- param "url"
    spanStr <- param "span"
    let id = read idStr :: Integer
    let port = read portStr :: Integer
    let span = read spanStr :: Integer
    liftIO $ updateFeedBot FeedBot {
      botId = id,
      ircServer = server,
      ircPort = port,
      ircChannel = channel,
      ircUser = user,
      feedUrl = url,
      feedSpan = span,
      processId = 0
      }
    redirect $ mconcat ["/bots/", pack idStr]

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
      "<a href='/bots'>Back</a>",
      "</form>",
      "</body>",
      "</html>"
      ]

  post "/bots/:id/delete" $ do
    id <- param "id"
    liftIO $ deleteFeedBot (read id)
    redirect "/bots"

  post "/bots/:id/execute" $ do
    id <- param "id"
    liftIO $ executeFeedBot (read id)
    redirect "/bots"

  post "/bots/:id/terminate" $ do
    id <- param "id"
    liftIO $ terminateFeedBot (read id)
    redirect "/bots"
