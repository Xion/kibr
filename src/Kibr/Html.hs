{-# LANGUAGE OverloadedStrings #-}

module Kibr.Html where

import Preamble

import Data.Lens
import Control.Monad.Identity
import Text.Groom
import Text.XHtmlCombinators.Strict
import Text.XHtmlCombinators.Strict.Attributes hiding (title)
import Text.XHtmlCombinators.Internal
import Web.Routes

import Language.Haskell.HsColour.ACSS (hscolour)

import Kibr.Data.Sitemap

import qualified Data.Text as T
import qualified Kibr.Data as DB

type View x = RouteT Sitemap (XHtmlMT x Identity) ()

linkCss :: (Functor t, Monad t) => T.Text -> XHtmlT t HeadContent
linkCss url = link' [href url, rel "stylesheet", type_ "text/css"]

linkTo :: (Functor t, Monad t, XHtml9 c) => T.Text -> XHtmlT t AContent -> XHtmlT t c
linkTo url = a' [href url]

master :: View BlockContent -> View Root
master page =
  do
    stylesheet <- showURL Stylesheet
    html $ do
      head_ $ do
        title "Lojban Dictionary"
        linkCss $ T.pack stylesheet
        linkCss hscolourCss
      body page
  where
    hscolourCss = "http://code.haskell.org/~malcolm/hscolour/hscolour.css"

wordList :: [DB.Word] -> View BlockContent
wordList ws =
  dl . forM_ ws $ \w -> do
    let word = DB.word ^$ w
    wurl <- showURL $ Word word
    dt . linkTo (T.pack wurl) $ word
    dd . hscolour False $ groom w
