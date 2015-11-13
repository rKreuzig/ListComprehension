module Handler.LstComExplanation where

import Import

getLstComExplanationR :: Handler Html
getLstComExplanationR =
  defaultLayout $ do
    setTitle "ListComprehension"
    $(widgetFile "lstcom-explanation")
