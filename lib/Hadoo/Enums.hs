module Hadoo.Enums where

data State = Todo | Started | Done deriving (Show, Eq, Bounded, Enum)

values :: (Bounded a, Enum a) => [a]
values = [minBound .. maxBound]