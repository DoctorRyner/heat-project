module App where

data User = User { name, password :: String }

defaultUser :: User
defaultUser = User "Keker" "deadspase2"

name' :: String
name' = defaultUser.password
