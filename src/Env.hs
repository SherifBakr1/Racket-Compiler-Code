module Env where

import qualified Data.List
-- is this what we want it to be?

emptyEnv :: [a]
emptyEnv = [] 

lookup :: Eq k => k -> [(k,v)] -> Maybe v
lookup = Data.List.lookup


bind :: k -> v -> [(k,v)] -> [(k,v)]
bind k v env = (k,v) : env