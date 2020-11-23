{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
--import Network.HTTP.Types.Status
import Database.Persist.Postgresql

getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
    [whamlet| 
            <h1>
                SISTEMA DE DOAÇÃO

            <ul>
                <li>
                    <a href=@{ProdutoR}>
                        CADASTRO DE PRODUTO PARA DOAÇÃO
    |]
