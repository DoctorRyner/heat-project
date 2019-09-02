module App where

import           Miso

data Model = Model {} deriving (Show, Eq)

defaultModel :: Model
defaultModel = Model

data Event
    = NoEvent

update :: Model -> Event -> Effect Event Model
update model = \case
    NoEvent -> pure model

view :: Model -> View Event
view _ = "Hello Haskell GUI ~"

app :: App Model Event
app = App
    { initialAction = NoEvent
    , model         = defaultModel
    , update        = flip App.update
    , view          = App.view
    , events        = defaultEvents
    , subs          = []
    , mountPoint    = Nothing
    }
