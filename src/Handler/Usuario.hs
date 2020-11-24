{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Tool
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
    (widget,_) <- generateFormPost formUsuario
    msg <- getMessage
    defaultLayout $ do 
        toWidgetHead $(luciusFile "templates/registrar.lucius") 
        $(whamletFile "templates/registrar.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formUsuario
    case result of 
        FormSuccess (usuario,veri) -> do
            if (usuarioSenha usuario == veri) then do
                runDB $ insert400 usuario
                setMessage [shamlet|
                    <div>
                         Usuario cadastrado com sucesso!!!
                |]
                redirect UsuarioR
            else do
                setMessage [shamlet|
                    <div>
                         Senha e veficação não coincidem!!!
                |]
                redirect UsuarioR
        _ -> redirect HomeR