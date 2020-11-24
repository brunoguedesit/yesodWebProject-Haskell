{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Retirar where

import Import
import Tool
import Text.Lucius
import Database.Persist.Sql


getListRetiradosR :: Handler Html
getListRetiradosR = do
    sess <- lookupSession "_EMAIL"
    case sess of
        Nothing -> redirect HomeR
        Just email -> do
            usuario <- runDB $ getBy (UniqueEmail email)
            case usuario of 
                 Nothing -> redirect HomeR
                 Just (Entity uid usuario) -> do 
                     let sql = "SELECT ??,??,?? FROM usuario \
                        \ INNER JOIN retirar ON retirar.usuarioid = usuario.id \
                        \ INNER JOIN produto ON retirar.produtoid = produto.id \
                        \ WHERE usuario.id = ?"
                     produtos <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario,Entity Retirar,Entity Produto)]
                     defaultLayout $ do 
                         toWidgetHead $(luciusFile "templates/retirados.lucius")
                         $(whamletFile "templates/retirados.hamlet")


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
