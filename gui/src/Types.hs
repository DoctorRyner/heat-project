module Types where

data Event
    = NoEvent

data Model = Model {} deriving (Show, Eq)

defaultModel :: Model
defaultModel = Model
