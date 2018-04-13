module Index (index, posts) where

import Prelude
import Data.Date
import Data.Foldable (for_)
import Types (PostMeta, PostRaw, PostContents, Post, mkDate)
import Tmpl (template, codeblock, linkTo)
import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes
import Text.Smolder.Markup
import Data.Maybe (Maybe, fromMaybe)

import Hljs (highlight)
import Katex (katex)
import Data.Tuple.Nested (type (/\), (/\))

import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A
import Data.Array (mapWithIndex)

import Posts.ExamplePost as EP

posts :: Array Post
posts = [EP.post]

index :: forall e. Markup e
index = template $ do
    p $ do
        strong $ text "Impure"
        text " is a PureScript static site generator. "
        text "It has one niche and goal: every page or HTML template "
        text "are to be written in PureScript."
    p $ do
        text "For example, the above portion of this page were written in the source as:"
    codeblock "haskell" """index :: forall e. Markup e
index = template $ do
    p $ do
        strong $ text "Impure"
        text " is a PureScript static site generator. "
        text "It has one niche and goal: every page or HTML template "
        text "are to be written in PureScript." """
    h2 $ text "here are some of my recent blogposts"
    ol $ do
        for_ (mapWithIndex (/\) posts) \(i /\ (m /\ _)) -> li $ linkTo m