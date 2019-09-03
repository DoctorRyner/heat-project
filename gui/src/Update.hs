module Update where

import           Miso
import           Types

update :: Model -> Event -> Effect Event Model
update model = \case
    NoEvent -> pure model
