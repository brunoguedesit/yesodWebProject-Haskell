{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Produto where

import Import
--import Database.Persist.Postgresql

formProduto :: Maybe Produto -> Form Produto
formProduto prod = renderDivs $ Produto  
    <$> areq textField (FieldSettings "Nome: " 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","myClass")]
                       ) (fmap produtoNome prod)
    <*> areq intField "Quantidade: " (fmap produtoQuantidade prod)

auxProdutoR :: Route App -> Maybe Produto -> Handler Html
auxProdutoR rt produto = do
    (widget,_) <- generateFormPost (formProduto Nothing)
    defaultLayout $ do
        [whamlet|
            <h1>
                Cadastre um produto para doação

            <form action=@{ProdutoR} method=post>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

getProdutoR :: Handler Html
getProdutoR = auxProdutoR ProdutoR Nothing

postProdutoR :: Handler Html
postProdutoR = do 
    ((resp,_),_)  <- runFormPost (formProduto Nothing)
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

getListProdR :: Handler Html
getListProdR = do 
    -- produtos :: [Entity Produto]
    produtos <- runDB $ selectList [] [Desc ProdutoQuantidade]
    defaultLayout [whamlet|
            <table>
                <thead>
                    <tr>
                        <th> 
                            Nome
                        
                        <th>
                            Qtd
                        
                        <th>
                        
                        <th>
                <tbody>
                    $forall Entity pid prod <- produtos
                        <tr>
                            <td>
                                <a href=@{DescR pid}>
                                    #{produtoNome prod}
                            
                            <td>
                                #{produtoQuantidade prod}
                            <th>
                                <a href=@{UpdProdR pid}>
                                    Editar
                    |]

getUpdProdR :: ProdutoId -> Handler Html
getUpdProdR pid = do 
    antigo <- runDB $ get404 pid
    auxProdutoR (UpdProdR pid) (Just antigo)    
    
postUpdProdR :: ProdutoId -> Handler Html
postUpdProdR pid = do
    ((resp,_),_) <- runFormPost (formProduto Nothing)
    case resp of 
         FormSuccess novo -> do
            runDB $ replace pid novo
            redirect (DescR pid) 
         _ -> redirect HomeR
