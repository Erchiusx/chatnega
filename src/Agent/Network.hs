module Agent.Network where

import Agent.Types
  ( Agent (..),
    Message,
    Model (api'key, name, url),
    Model'Response,
    State (history),
    response'to'message,
  )
import Data.Aeson (KeyValue ((.=)), object)
import Network.HTTP.Req
  ( MonadHttp,
    POST (POST),
    Req,
    ReqBodyJson (ReqBodyJson),
    defaultHttpConfig,
    header,
    jsonResponse,
    req,
    responseBody,
    runReq,
  )

ask :: forall state m. (MonadHttp m) => Model -> Message -> Agent state m Model'Response
ask mdl msg = Agent $ \state succ kfail -> do
  b <- runReq defaultHttpConfig $ do
    let payload =
          object
            [ "model" .= mdl.name,
              "messages" .= (state.history ++ [msg])
            ]
        headers =
          header "Content-Type" "application/json"
            <> header "Authorization" ("Bearer " <> mdl.api'key)

    responseResult <-
      req
        POST
        mdl.url
        (ReqBodyJson payload)
        jsonResponse
        headers

    return $ (responseBody responseResult :: Model'Response)
  b
    `succ` state
      { history = state.history ++ [msg, response'to'message b]
      }

runIO :: State s -> Agent s Req a -> IO a
runIO st ag = runReq defaultHttpConfig (runAgent ag st (\v _ -> return v) (\e _ -> error e))
