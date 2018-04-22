module Impur.Index (index, posts, categoryPage) where

import Control.Monad.ST
import Data.Date
import Prelude
import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes
import Text.Smolder.Markup

import Data.Array (mapWithIndex, filter, sortBy)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit)
import Impur.Conf (Category(..), categories)
import Impur.Hljs (highlight)
import Impur.Katex (katex)
import Impur.Posts.ExamplePost as EP
import Impur.Tmpl (codeblock, linkTo, template, categoryLink, math, mathblock)
import Impur.Types (PostMeta, Post,  mkDate)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A

posts :: Array Post
posts = [EP.post]

categoryCount :: Category -> Int
categoryCount cat = pureST do
    cnt <- newSTRef 0
    for_ posts $ \(m /\ _) ->
        modifySTRef cnt (\x -> x + if m.category == Just cat then 1 else 0)
    readSTRef cnt

categoryPage :: forall e. Category -> Markup e
categoryPage cat = template $ do
    h2 $ text $ "Posts with Category: " <> show cat
    ol $ for_ posts \(m /\ _) -> li $
            if m.category == Just cat then linkTo m else pure unit

index :: forall e. Markup e
index = template $ do
    H.div $ p $ do
        mathblock "\\sqrt[3]{x}"
    p $ do
        strong $ text "Impur"
        text " is a PureScript static site generator. "
        text "It has one niche and goal: every page or HTML template "
        text "are to be written in PureScript."
    p $ do
        text "For example, the above portion of this page were written in the source as:"
    codeblock "haskell" """index :: forall e. Markup e
index = template $ do
    p $ do
        strong $ text "Impur"
        text " is a PureScript static site generator. "
        text "It has one niche and goal: every page or HTML template "
        text "are to be written in PureScript." """
    h2 $ text "here are some of my recent blogposts"
    ol $ do
        for_ (mapWithIndex (/\) posts) \(i /\ (m /\ _)) -> li $ linkTo m
    h2 $ text "Categories"
    p $
        for_ categories \c -> do
            categoryLink c $ Just (categoryCount c)
            text " "