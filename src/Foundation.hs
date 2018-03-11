{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Foundation where

import Yesod.Core

data App = App

mkMessage "App" "messages" "en"
