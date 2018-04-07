module Index (index, posts) where

import Prelude
import Data.Date
import Data.Foldable (for_)

import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes
import Text.Smolder.Markup
import Data.Maybe (Maybe, fromMaybe)
import Data.Enum (toEnum)
import Hljs (highlight)
import Katex (katex)
import Data.Tuple.Nested (type (/\), (/\))

import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A

type PostMeta = {
    title :: String,
    published :: Maybe Date
}

type PostRaw = forall e. Markup e

type PostContents = PostMeta -> PostRaw

type Post = forall a. PostMeta /\ (PostMeta -> Markup a)

mkDate :: Int -> Int -> Int -> Maybe Date
mkDate y m d = do
    y' <- toEnum y
    m' <- toEnum m
    d' <- toEnum d
    pure $ canonicalDate y' m' d'


testMeta :: PostMeta
testMeta = {title: "Example Post", published: mkDate 2018 4 2}

testContents :: PostContents
testContents = \_ -> index

examplePost :: Post
examplePost = testMeta /\ samplePost

blogTemplate :: forall a. Markup a -> PostMeta -> Markup a
blogTemplate f meta =
    template $ do
        h1 $ text meta.title
        f

samplePost :: forall a. PostMeta -> Markup a
samplePost = blogTemplate $ do
    p $ text "this is my first paragraph"


reifyContents :: forall e. Post -> Html e
reifyContents (a /\ b) = b a

posts :: Array Post
posts = [examplePost]

template :: forall a. Markup a -> Markup a
template partial = html ! lang "en" $ do
    H.head $ do
        link ! rel "stylesheet" ! href "/hack.css"
        link ! rel "stylesheet" ! href "/highlight.min.css"
    body ! className "hack" $ do
        H.div ! className "container" $ do
            partial

codeblock :: forall e. String -> String -> Markup e
codeblock a b = pre $ code $ unsafeRawText $ highlight a b

index :: forall e. Markup e
index = template $ do
    h1 $ text "Homot, a purescript static site generator"
    h2 $ text "here are some of my recent blogposts"
    codeblock "ruby" "puts 1, 2, 3"
    -- H.div $ unsafeRawText $ fromMaybe "Katex Error" $ katex "c = \\pm\\sqrt{a^2 + b^2}"
    for_ posts \({title: title} /\ _) -> p $ text title