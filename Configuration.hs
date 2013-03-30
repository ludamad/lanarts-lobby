{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

module Configuration where

import Data.Text as T
import Network (PortID(..))

data Configuration = Configuration {
        databaseIP :: T.Text
        ,serverPort :: PortID
}

defaultConfiguration :: Configuration
defaultConfiguration = Configuration { 
    databaseIP = T.pack "localhost"
    ,serverPort = PortNumber 6113
}
