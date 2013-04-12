module Session
where

import qualified System.Time as ST
import Database.MongoDB (Document, (=:), cast, Binary(..), ObjectId(..) )
import UserStats

data Session = Session {
    sessionId :: ObjectId
    , sessionUser :: User
    , sessionLastUpdate :: Integer
}


