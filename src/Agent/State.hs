module Agent.State (get'state, set'state) where

import Agent.Types

get'state :: Agent s u s
get'state = Agent $ \s succ _ ->
  s.user'state `succ` s

set'state :: s -> Agent s u ()
set'state a = Agent $ \s succ _ ->
  () `succ` s {user'state = a}
