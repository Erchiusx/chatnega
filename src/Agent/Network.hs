module Agent.Network (ask, runIO) where

import Agent.Types
  ( Agent (..),
    Message,
    Model (api'key, name, url),
    Model'Response (message),
    State (history),
  )
import Control.Exception (try)
import Control.Monad.IO.Class
import Data.Aeson (KeyValue ((.=)), object)
import Network.HTTP.Req
  ( HttpException,
    MonadHttp,
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

ask :: (MonadHttp m) => Model -> Message -> Agent state m Model'Response
ask model message = Agent $ \state succ failF -> do
  resultOrErr <- tryHttp $ sendRequest state

  case resultOrErr of
    Right response ->
      response `succ` state {history = state.history ++ [message, response.message]}
    Left e ->
      show e `failF` state
  where
    sendRequest :: State state -> IO Model'Response
    sendRequest state = runReq defaultHttpConfig $ do
      let payload =
            object
              [ "model" .= model.name,
                "messages" .= (state.history ++ [message])
              ]
          headers =
            header "Content-Type" "application/json"
              <> header "Authorization" ("Bearer " <> model.api'key)

      responseResult <- req POST model.url (ReqBodyJson payload) jsonResponse headers
      return $ responseBody responseResult

tryHttp :: (MonadIO m) => IO a -> m (Either HttpException a)
tryHttp action = liftIO $ try @HttpException action

-- ask' :: forall state m. MonadHttp m => Model -> Message -> Agent state m Model'Response
-- ask' mdl msg = Agent $ \state succ failF ->
--   runReq defaultHttpConfig
--     $ handleHttpException
--       (\e -> liftIO $ failF (show e) state >> E.throwIO e)
--     $ do
--       -- 捕获 HTTP 错误后手动 fail
--       let payload =
--             object
--               [ "model" .= mdl.name,
--                 "messages" .= (state.history ++ [msg])
--               ]
--           headers =
--             header "Content-Type" "application/json"
--               <> header "Authorization" ("Bearer " <> mdl.api'key)

--       responseResult <-
--         req
--           POST
--           mdl.url
--           (ReqBodyJson payload)
--           jsonResponse
--           headers

--       let response = responseBody responseResult :: Model'Response
--       liftIO $ succ response state {history = state.history ++ [msg, response.message]}

runIO :: State s -> Agent s Req a -> IO a
runIO st ag = runReq defaultHttpConfig (runAgent ag st (\v _ -> return v) (\e _ -> error e))
