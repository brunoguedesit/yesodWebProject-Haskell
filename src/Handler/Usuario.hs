{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Text.Lucius

formUsuario :: Form (Usuario, Text)
formUsuario = renderBootstrap $ (,)
    <$> (Usuario
        <$> areq textField "Nome: " Nothing
        <*> areq emailField "E-mail: " Nothing
        <*> areq passwordField "Senha: " Nothing)
    <*> areq passwordField "Digite Novamente: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
    rt <- return UsuarioR
    (widget,_) <- generateFormPost formUsuario
    msg <- getMessage
    defaultLayout $ do 
        rt <- return UsuarioR
        titulo <- return "CADASTRO DE USUARIO"
        btn <- return "Cadastrar"
        toWidgetHead $(luciusFile "templates/form.lucius") 
        $(whamletFile "templates/form.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formUsuario
    case result of 
        FormSuccess (usuario,veri) -> do
            if (usuarioSenha usuario == veri) then do
                runDB $ insert400 usuario
                setMessage [shamlet|
                    <div>
                         USUARIO INCLUIDO
                |]
                redirect UsuarioR
            else do
                setMessage [shamlet|
                    <div>
                         SENHA E VERIFICACAO NAO COINCIDEM
                |]
                redirect UsuarioR
        _ -> redirect HomeR