{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Produto where

import Import
--import Database.Persist.Postgresql

formProduto :: Form Produto
formProduto = renderDivs $ Produto
    <$> areq textField (FieldSettings "Nome: " 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","myClass")]
                        ) Nothing
    <*> areq intField "Quantidade: " Nothing

getProdutoR :: Handler Html
getProdutoR = do
    (widget,_) <- generateFormPost formProduto
    defaultLayout $ do
        [whamlet|
            <h1>
                Cadastre um produto para doação

            <form action=@{ProdutoR} method=post>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postProdutoR :: Handler Html
postProdutoR = do 
    ((resp,_),_)  <- runFormPost formProduto
    case resp of
        FormSuccess produto -> do
            pid <- runDB $ insert produto
            redirect (DescR pid)
        _ -> redirect HomeR

getDescR :: ProdutoId -> Handler Html
getDescR pid = do
    produto <- runDB $ get404 pid
    defaultLayout [whamlet|
        <h1>
            Nome: #{produtoNome produto}
        <h2>
            Quantidade: #{produtoQuantidade produto}
                  |]