module Agent.Types
  ( State (..),
    Message (..),
    Model'Response (..),
    Agent (..),
    Model (..),
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON,
    withObject,
    (.:),
    (.:?),
  )
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Network.HTTP.Req
  ( MonadHttp (..),
    Scheme (Https),
    Url,
  )

data State s = State
  { history :: [Message],
    user'state :: s
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

instance ToJSON Model'Response

instance FromJSON Model'Response where
  parseJSON val = do
    arr <- withObject "Total_Response" (\v -> v .: "choices") val
    let res = arr V.! 0
    withObject
      "Model'Response"
      ( \v ->
          Model'Response
            <$> v .: "finish_reason"
            <*> v .: "index"
            <*> v .:? "logprobs"
            <*> v .: "message"
      )
      res

newtype Agent state m value = Agent
  { runAgent ::
      forall b.
      State state ->
      (value -> State state -> m b) -> -- on success
      (String -> State state -> m b) -> -- on failure
      m b
  }

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

instance MonadFail (Agent state m) where
  fail msg = Agent $ \s _ fail ->
    fail msg s

instance MonadTrans (Agent state) where
  lift ma = Agent $ \s succ _ ->
    ma >>= \v -> v `succ` s

instance (MonadIO m) => MonadIO (Agent state m) where
  liftIO io = Agent $ \s succ _ ->
    liftIO io >>= \v -> succ v s

instance (MonadHttp m) => MonadHttp (Agent state m) where
  handleHttpException = lift . handleHttpException
  getHttpConfig = lift getHttpConfig

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
