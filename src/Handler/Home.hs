{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Text.Lucius
import Database.Persist.Postgresql

getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
    toWidgetHead $(luciusFile "templates/homepage.lucius")
    $(whamletFile "templates/homepage.hamlet")
    
