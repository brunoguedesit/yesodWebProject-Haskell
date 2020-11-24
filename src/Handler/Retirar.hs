{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Retirar where

import Import

postRetirarR :: ProdutoId -> Handler Html
postRetirar pid = 
    ((resp,_),_)  <- runFormPost formQt
    case resp of 
        _ -> redirect HomeR
        FormSuccess qt -> do
            sess <- lookupSession "_EMAIL"
            case sess of
                Nothing -> redirect HomeR
                Just email -> do
                    email <- runDN $ getBy (UniqueEmail email)
                    case usuario of 
                        Nothing -> redirect HomeR
                        Just (Entity uid _) -> do
                            runDB $ insert (Retirar pid uid qt)
                            redirect HomeR
