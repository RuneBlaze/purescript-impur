module Impur.Index (index, posts, categoryPage) where


import Data.Date
import Prelude
import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes
import Text.Smolder.Markup
import Data.Array (mapWithIndex)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit)
import Impur.Conf (Category(..), categories)
import Impur.Hljs (highlight)
import Impur.Katex (katex)
import Impur.Posts.ExamplePost as EP
import Impur.Tmpl (codeblock, linkTo, template, categoryLink, math, mathblock, faIcon)
import Impur.Types (PostMeta, Post,  mkDate, GMeta, GPost, categoryCount)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A
import Impur.Classes (class TagLike)

showMeta :: forall t e . (TagLike t) => {category :: Maybe t | e} -> Maybe String
showMeta m = case m.category of
    Just n -> pure $ show n
    Nothing -> Nothing

posts :: Array ({category :: Maybe Category, published :: Maybe Date, title :: String} /\ ({category :: Maybe Category, published :: Maybe Date, title :: String } -> forall e. Markup e))
posts = [EP.post]

categoryPage :: forall t r c. (TagLike t) => t -> Array ({category :: Maybe t, published :: Maybe Date, title :: String | r} /\ c) -> forall e. Markup e
categoryPage cat psts = template $ do
    h2 $ text $ "Posts with Category: " <> show cat
    ol $ for_ psts \(m /\ _) -> li $
            if (show <$> (m.category :: Maybe t)) == (show <$> Just cat) then linkTo m else pure unit

index :: forall t r a e. (TagLike t) => Array ({category :: Maybe t | r} /\ a) -> Markup e
index psts = template $ do
    p $ faIcon "twitter"
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
            categoryLink c $ Just (categoryCount c psts)
            text " "