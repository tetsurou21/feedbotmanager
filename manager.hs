{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Data.Monoid (mconcat)
import Data.Text.Lazy (pack)
import Control.Monad.IO.Class
import Text.CSV

main = scotty 3000 $ do
  get "/bots" $ do
    contents <- liftIO $ parseCSVFromFile "bots.csv"
    html $ mconcat [
      "<div>",
      pack $ show contents,
      "</div>"
      ]
