module Agent.Types
  ( State(..)
  , Message(..)
  , Model'Response(..)
  , Agent(..)
  , Model(..)
  , getState
  , ask
  , runIO
  ) where


import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.ByteString
import Data.Text
import GHC.Generics (Generic)
import Network.HTTP.Req
import Data.Vector qualified as V

data State s = State
  { history :: [Message],
    stateUser :: s
  }

data Message = Message
  { role :: Text,
    content :: Text
  }
  deriving (Show, Generic)

instance ToJSON Message

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v ->
    Message
      <$> v .: "role"
      <*> v .: "content"

data Model'Response = Model'Response
  { finish_reason :: Text,
    index :: Int,
    logprobs :: Maybe String,
    message :: Message
  }
  deriving (Show, Generic)

response'to'message :: Model'Response -> Message
response'to'message res = res.message


instance ToJSON Model'Response
instance FromJSON Model'Response where
  parseJSON val = do
    arr <- withObject "Total_Response" (\v -> v .: "choices") val
    let res = arr V.! 0
    withObject "Model'Response" ( \v ->
      Model'Response
        <$> v .: "finish_reason"
        <*> v .: "index"
        <*> v .:? "logprobs"
        <*> v .: "message"
      ) res

newtype Agent state m value = Agent
  { runAgent ::
      forall b.
      State state ->
      (value -> State state -> m b) -> -- on success
      (String -> State state -> m b) -> -- on failure
      m b
  }

getState :: (Monad m) => Agent s m s
getState = Agent $ \state succ _ ->
  stateUser state `succ` state

instance Functor (Agent state m) where
  fmap f agent = Agent $ \s succ kfail ->
    runAgent
      agent
      s
      (\v s' -> succ (f v) s')
      (\err s' -> kfail err s')

instance Applicative (Agent state m) where
  pure x = Agent $ \s succ _ -> succ x s
  af <*> ax = do
    f <- af
    x <- ax
    return (f x)
  (<*) = liftA2 const
  (*>) = liftA2 $ const id

instance Monad (Agent state m) where
  return = pure
  Agent m >>= f = Agent $ \s succ kfail ->
    m
      s
      (\v s' -> runAgent (f v) s' succ kfail)
      (\err s' -> kfail err s')

data Model = Agent'Model
  { name :: String,
    url :: Url 'Https,
    api'key :: ByteString
  }

instance Show Model where
  show Agent'Model {..} =
    "Agent'Model {"
      <> " name = "
      <> show name
      <> ", url = "
      <> show url
      <> " }"

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
  b `succ` state {
    history = state.history ++ [msg, response'to'message b]
  }

runIO :: State s -> Agent s Req a -> IO a
runIO st ag = runReq defaultHttpConfig (runAgent ag st (\v _ -> return v) (\e _ -> error e))
