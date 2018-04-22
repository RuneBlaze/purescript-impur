module Impur.Conf (author, Category(..), categories) where

import Prelude

author :: String
author = "Gilgamesh"

data Category = Programming | Music

categories :: Array Category
categories = [Programming, Music]

instance showCategory :: Show Category where
    show Programming = "Programming"
    show Music = "Music"

derive instance eqCategory :: Eq Category