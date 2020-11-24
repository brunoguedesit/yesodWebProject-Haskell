{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Menu where

import Import
import Database.Persist.Postgresql
import Text.Lucius

getMenuR :: Handler Html
getMenuR = defaultLayout $ do 
    addStylesheet (StaticR css_bootstrap_css)
    sess <- lookupSession "_EMAIL"
    toWidgetHead $(luciusFile "templates/menu.lucius")
    $(whamletFile "templates/menu.hamlet")
    
