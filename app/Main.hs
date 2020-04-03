{-# LANGUAGE OverloadedStrings #-}
import           Network.HostName
import           Control.Applicative
import           Data.Maybe
import           Network.Wai.Handler.Warp
import           Data.Time
import           Lucid
import           Lexx
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import           Data.Text (Text)
import           System.Environment
import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp (run)

app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app request respond = do
  if rawPathInfo request == "/ping"
    then respond $ responseLBS status200 [("Content-Type", "text/plain")] mempty
    else do
      now <- getCurrentTime
      env <- getEnvironment
      hostname <- getHostName
      respond $
        responseLBS
          status200
          [("Content-Type", "text/html; charset=utf-8")]
          (renderBS
             (doctypehtml_
                (do head_
                      (do title_ "lbtest: OK!"
                          style_ css)
                    body_
                      (do h1_ "lbtest: OK"
                          p_
                            (do "Hostname: "
                                toHtml hostname)
                          p_
                            (do "Timestamp: "
                                toHtml (show now))
                          h2_ "Request"
                          pre_ (commandsToHtml (lexx (show request)))
                          h2_ "Environment"
                          pre_ (commandsToHtml (lexx (show env)))))))

main :: IO ()
main = do
  port <- getEnv "PORT"
  setProxyMaybe <-
    do mproxyrequired <-
         fmap
           (fmap (const setProxyProtocolRequired))
           (lookupEnv "PROXY_REQUIRED")
       mproxyoptional <-
         fmap
           (fmap (const setProxyProtocolOptional))
           (lookupEnv "PROXY_OPTIONAL")
       pure (fromMaybe id (mproxyoptional <|> mproxyoptional))
  runSettings (setProxyMaybe (setPort (read port) defaultSettings)) app

css :: Text
css =
  ".lexx-delimiter {\n\
  \  color: #449a86\n\
  \}\n\
  \.lexx-constructor {\n\
  \  color: #44449a\n\
  \}\n\
  \.lexx-misc {\n\
  \  color: #444;\n\
  \}\n\
  \.lexx-string {\n\
  \  color: #8d449a;\n\
  \}\n\
  \.lexx-digits {\n\
  \  color: #349a91;\n\
  \}\n\
  \"
