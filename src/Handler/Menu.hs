{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Menu where

import Import
import Database.Persist.Postgresql

getMenuR :: Handler Html
getMenuR = defaultLayout $ do 
    addStylesheet (StaticR css_bootstrap_css)
    sess <- lookupSession "_EMAIL"
    [whamlet| 
            <h1>
                SISTEMA DE DOAÇÃO
            <ul>
                <li>
                    <a href=@{ProdutoR}>
                        CADASTRO DE PRODUTO PARA DOAÇÃO

                <li>
                    <a href=@{ListProdR}>
                        LISTAR PRODUTOS DISPONÍVEIS PARA DOAÇÃO
        
                $maybe email <- sess
                    <li>
                        <div>
                            #{email}
                            <form method=post action=@{SairR}>
                                <input type="submit" value="Sair">
                $nothing
                    <li>
                        <a href=@{EntrarR}>
                            LOGAR
    |]