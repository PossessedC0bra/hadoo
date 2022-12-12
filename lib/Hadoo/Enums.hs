module Hadoo.Enums where

data State = Todo | Started | Done deriving (Show, Read, Eq, Ord, Enum, Bounded)

values :: (Bounded a, Enum a) => [a]
values = [minBound .. maxBound]