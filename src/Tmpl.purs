module Tmpl (template, blogTemplate, codeblock, linkTo) where

import Prelude
import Data.Date
import Data.Foldable (for_)
import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes
import Text.Smolder.Markup
import Data.Maybe (Maybe, fromMaybe)
import Data.Enum (toEnum)
import Hljs (highlight)
import Limax (limax)
import Katex (katex)
import Data.Tuple.Nested (type (/\), (/\))
import Types (PostMeta)
import Text.Smolder.HTML as H


template :: forall a. Markup a -> Markup a
template partial = html ! lang "en" $ do
    H.head $ do
        H.title $ text "Impure, a PureScript static site generator"
        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1"
        link ! rel "stylesheet" ! href "/hack.css"
        link ! rel "stylesheet" ! href "/highlight.min.css"
    body ! className "hack" $ do
        H.div ! className "container" $ do
            h1 $ text "Homot, a purescript static site generator"
            nav $ do
                H.div ! className "cell -4of12" $ do
                    H.div ! className "content" $ do
                        p $ a ! href "/" $ text "Index"
            partial

blogTemplate :: forall a. Markup a -> PostMeta -> Markup a
blogTemplate f meta =
    template $ do
        h2 $ text meta.title
        f

codeblock :: forall e. String -> String -> Markup e
codeblock a b = pre $ code $ unsafeRawText $ highlight a b

linkTo :: forall a. PostMeta -> Markup a
linkTo m = a ! href ("/posts/" <> limax m.title) $ text m.title