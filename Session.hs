module Session where

import qualified Data.Text as T

data UserData = UserData { username :: T.Text }

nullUser :: UserData
nullUser = UserData { username = T.pack "" }

data Session = Session { userData :: UserData }

nullSession :: Session
nullSession = Session { userData = nullUser }

