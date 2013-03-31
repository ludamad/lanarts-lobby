{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

module Configuration where

import qualified Data.Text as T
import qualified Network as Net 

data Configuration = Configuration {
        databaseIP :: T.Text
        ,serverPort :: Net.PortID
        ,handleSigINT :: Bool
}

defaultConfiguration :: Configuration
defaultConfiguration = Configuration { 
    databaseIP = T.pack "localhost"
    ,serverPort = Net.PortNumber 6113
    ,handleSigINT = False -- Should be true before deploying
}
