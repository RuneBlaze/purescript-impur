module Impur.Posts.ExamplePost (post) where

import Prelude (($))
import Impur.Types (PostMeta, PostRaw, PostContents, Post, mkDate)
import Impur.Tmpl (blogTemplate)
import Text.Smolder.Markup
import Data.Date
import Data.Tuple.Nested ((/\))
import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes

postMeta :: PostMeta
postMeta = {title: "Example Post", published: mkDate 2018 4 2}

contents :: forall a. PostMeta -> Markup a
contents = blogTemplate $ do
    p $ text "this is my first paragraph"

post :: Post
post = postMeta /\ contents