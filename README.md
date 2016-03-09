# zim-parser: Haskell library for parsing ZIM files

This is a library for parsing ZIM (http://openzim.org) files.
ZIM files contain offline web content (eg, Wikipedia) which
can be browsed locally without an Internet connection.

ZIM files of Wikimedia Foundation (Wikipedia, Wikibooks, etc) can be
found at http://ftpmirror.your.org/pub/kiwix/zim.

Below is a full example of a Scotty web server that serves a ZIM file
(specified on command line) on localhost port 3000:
```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Environment (getArgs)
import Network.HTTP.Types.Status (status404)
import Web.Scotty
import Codec.Archive.Zim.Parser (getMainPageUrl, getContent, Url(..))

main :: IO ()
main = do
    [fp] <- getArgs
    scotty 3000 $ do
      get "/" (redirectToZimMainPage fp)
      get (regex "^/(./.*)$") (serveZimUrl fp)
      notFound $ text "Invalid URL!"

redirectToZimMainPage :: FilePath -> ActionM ()
redirectToZimMainPage fp = do
    res <- liftIO $ getMainPageUrl fp
    case res of
      Nothing -> do
        status status404
        text "This ZIM file has no main page specified!"
      Just (Url url) -> redirect . fromStrict $ decodeUtf8 url

serveZimUrl :: FilePath -> ActionM ()
serveZimUrl fp = do
    url <- (encodeUtf8 . toStrict) <$> param "1"
    res <- liftIO $ fp `getContent` Url url
    case res of
      Nothing -> do
        liftIO . putStrLn $ "Invalid URL: " ++ show url
        status status404
        text $ "Invalid URL!"
      Just (mimeType, content) -> do
        liftIO . putStrLn $ "Serving: " ++ show url
        setHeader "Content-Type" (fromStrict $ decodeUtf8 mimeType)
        raw content

```

Feedback and contributions are welcome.
