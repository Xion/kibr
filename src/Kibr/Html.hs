{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Kibr.Html where

import Preamble

import Data.Lens
import HSP
import Text.Groom

import Language.Haskell.HsColour.ACSS (hscolour)

import Kibr.Data.Sitemap

import qualified Kibr.Data as DB

master body =
  <html>
    <head>
      <title>Lojban Dictionary</title>
      <link href=Stylesheet rel="stylesheet" type="text/css"/>
      <link href="http://code.haskell.org/~malcolm/hscolour/hscolour.css" rel="stylesheet" type="text/css"/>
    </head>
    <body>
      <% body %>
    </body>
  </html>

wordList ws =
  <dl>
    <%
      forM_ ws $ \w -> do
        let word = DB.word ^$ w
        <dt><a href=(Word word)><% word %></a></dt>
        <dd><% hscolour False $ groom w %></dd>
    %>
  </dl>
