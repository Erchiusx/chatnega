module Agent.History (get'history, last'message, set'history) where

import Agent.Types

get'history :: Agent s m [Message]
get'history = Agent $ \s succ _ ->
  s.history `succ` s

last'message :: (Monad m) => Agent s m Message
last'message = Agent $ \s succ _ ->
  last s.history `succ` s

set'history :: [Message] -> Agent s m ()
set'history ss = Agent $ \s succ _ ->
  () `succ` s {history = ss}
