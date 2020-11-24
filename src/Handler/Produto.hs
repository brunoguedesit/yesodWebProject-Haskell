{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Produto where

import Import
import Tool
import Text.Lucius

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
        toWidgetHead $(luciusFile "templates/registrarProduto.lucius")
        $(whamletFile "templates/registrarProduto.hamlet")

getProdutoR :: Handler Html
getProdutoR = auxProdutoR ProdutoR Nothing

postProdutoR :: Handler Html
postProdutoR = do 
    ((resp,_),_)  <- runFormPost (formProduto Nothing)
    case resp of
        FormSuccess produto -> do
            pid <- runDB $ insert produto
            redirect (MenuR)
        _ -> redirect HomeR

getDescR :: ProdutoId -> Handler Html
getDescR pid = do
    produto <- runDB $ get404 pid
    (widget,_) <- generateFormPost formQt
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/desc.lucius")
        $(whamletFile "templates/desc.hamlet")

getListProdR :: Handler Html
getListProdR = do 
    produtos <- runDB $ selectList [] [Desc ProdutoQuantidade]
    defaultLayout $ do 
        toWidgetHead $(luciusFile "templates/listar.lucius")
        $(whamletFile "templates/listar.hamlet")

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
            redirect (MenuR) 
         _ -> redirect HomeR

postDelProdR :: ProdutoId -> Handler Html
postDelProdR pid = do
    _ <- runDB $ get404 pid
    runDB $ delete pid 
    redirect ListProdR
