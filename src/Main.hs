module Main where

import Agent.Network (ask, runIO)
import Agent.Types
  ( Message (Message),
    Model (Agent'Model, api'key, name, url),
    State (State),
  )
import Data.ByteString qualified as BS
import Network.HTTP.Req (https, (/:))

initState :: s -> State s
initState s = State [] s

main :: IO ()
main = do
  api'key <- BS.readFile "./api.key"
  let peiqi =
        Agent'Model
          { name = "gpt-3.5-turbo",
            url = https "api.chatanywhere.tech" /: "v1" /: "chat" /: "completions",
            api'key = api'key
          }
  res <- runIO (initState ()) $ ask peiqi $ Message "user" "Say this is a test!"
  print res
