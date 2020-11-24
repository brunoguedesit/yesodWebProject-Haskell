{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Retirar where

import Import
import Tool

getListRetiradosR :: Handler Html
getListRetiradosR = undefined

postRetirarR :: ProdutoId -> Handler Html
postRetirarR pid = do
    ((resp,_),_)  <- runFormPost formQt
    case resp of 
        FormSuccess qt -> do
            sess <- lookupSession "_EMAIL"
            case sess of
                Nothing -> redirect HomeR
                Just email -> do
                    usuario <- runDB $ getBy (UniqueEmail email)

                    case usuario of 
                        Nothing -> redirect HomeR
                        Just (Entity uid _) -> do
                            runDB $ insert (Retirar pid uid qt)
                            redirect ListRetiradosR
        _ -> redirect HomeR
