module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, Value, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req
  ( POST (POST)
  , ReqBodyJson (ReqBodyJson)
  , defaultHttpConfig
  , header
  , https
  , jsonResponse
  , req
  , responseBody
  , runReq
  , (/:)
  )

-- 定义消息结构
data Message = Message
  { role :: Text
  , content :: Text
  }
  deriving (Show, Generic)

instance ToJSON Message

-- 定义请求体结构
data RequestBody = RequestBody
  { model :: Text
  , messages :: [Message]
  , temperature :: Double
  }
  deriving (Show, Generic)

instance ToJSON RequestBody

main :: IO ()
main = do
  apiKey <- BS.readFile "./api.key"
  runReq defaultHttpConfig $ do
    let
      url =
        https "api.chatanywhere.tech"
          /: "v1"
          /: "chat"
          /: "completions"
      headers =
        header "Authorization" ("Bearer " <> apiKey)
          <> header "Content-Type" "application/json"
      requestBody =
        RequestBody
          { model = "gpt-3.5-turbo"
          , messages = [Message "user" "Say this is a test!"]
          , temperature = 0.7
          }

    r <-
      req
        POST
        url
        (ReqBodyJson requestBody)
        jsonResponse
        headers

    liftIO $ BL.putStr $ encode (responseBody r :: Value)
