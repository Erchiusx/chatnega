module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.ByteString qualified as BS
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req
import Agent.Types

initState :: s -> State s
initState s = State [] s

main :: IO ()
main = do
  api'key <- BS.readFile "./api.key"
  let peiqi = Agent'Model {
      name = "gpt-3.5-turbo",
      url = https "api.chatanywhere.tech" /: "v1" /: "chat" /: "completions",
      api'key = api'key
    }
  res <- runIO (initState ()) $ ask peiqi $ Message "user" "Say this is a test!"
  print res

