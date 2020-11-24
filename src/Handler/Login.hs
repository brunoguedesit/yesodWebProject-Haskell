{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Text.Lucius

formUsuario :: Form (Text, Text)
formUsuario = renderBootstrap $ (,)
        <$> areq emailField "E-mail: " Nothing
        <*> areq passwordField "Senha: " Nothing)

getEntrarR :: Handler Html
getEntrarR = do 
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do 
        rt <- return EntrarR
        titulo <- return "LOGIN"
        btn <- return "Entrar"
        $(whamletFile "templates/form.hamlet")
