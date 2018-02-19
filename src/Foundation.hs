{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Foundation where

import Yesod.Core
import Yesod.Default.Util
import Data.Default

data App = App

mkYesodData "App" $(parseRoutesFile "routes")

mkMessage "App" "messages" "en"

widgetFile = widgetFileNoReload def

instance Yesod App
