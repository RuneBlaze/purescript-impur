module Impur.Tmpl (template, blogTemplate, codeblock, linkTo, categoryLink, math, mathblock, faIcon) where

import Data.Date
import Prelude
import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes
import Text.Smolder.Markup
import Impur.FontAwesome (faIconRaw, faCss)
import Impur.Conf (author, Category(..))
import Data.Formatter.DateTime
import Data.Unit (Unit(..))
import Data.Enum (toEnum)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Impur.Hljs (highlight)
import Impur.Katex (katex)
import Impur.Limax (limax)
import Text.Smolder.HTML as H
import Text.Smolder.Markup (mapEvent)
import Impur.Types (PostMeta)
import Data.DateTime (Date, DateTime(..), Time)
import Data.List.Types (List(..), (:))
import Impur.Classes (class TagLike)

formatDate :: Date -> String
formatDate date =
    let dt = DateTime date (bottom :: Time) in
    let fmt = DayOfMonth  : Placeholder " " : MonthShort : Placeholder " " : (Cons YearFull Nil) in
    format fmt dt

template :: forall a. Markup a -> Markup a
template partial = html ! lang "en" $ do
    let heading = "Impur, a PureScript static site generator"
    H.head $ do
        H.title $ text heading
        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1"
        link ! rel "stylesheet" ! href "/hack.css"
        link ! rel "stylesheet" ! href "/highlight.min.css"
        link ! rel "stylesheet" ! href "/katex.min.css"
        link ! rel "stylesheet" ! href "/custom.css"
        H.style $ unsafeRawText $ faCss
    body ! className "hack" $ do
        H.div ! className "container" $ do
            h1 $ text heading
            nav $ do
                H.div ! className "cell -4of12" $ do
                    H.div ! className "content" $ do
                        p $ a ! href "/" $ text "Index"
            partial

blogTemplate :: forall a t e. Markup a -> (TagLike t) => {
    title :: String,
    published :: Maybe Date,
    category :: Maybe t | e
} -> Markup a
blogTemplate f meta =
    template $ do
        h2 $ text meta.title
        p $ do
            text "by "
            strong $ text $ author
            let d = meta.published
            case d of
                Nothing -> pure unit
                Just date -> do
                    text ", "
                    strong $ text $ formatDate date
        case meta.category of
            Nothing -> pure unit
            Just cat -> do
                p $ do
                    text "This post is categorized as: "
                    categoryLink cat Nothing
                    text "."

codeblock :: forall e. String -> String -> Markup e
codeblock a b = pre $ code $ unsafeRawText $ highlight a b

katexblock :: forall e. String -> Boolean -> Markup e
katexblock s b = fromMaybe (p $ text $ "katex error: " <> s) $ unsafeRawText <$> katex s b

math :: forall e. String -> Markup e
math s = katexblock s false

mathblock :: forall e. String -> Markup e
mathblock s = katexblock s true

categoryText :: forall t. (TagLike t) => t -> Maybe Int -> String
categoryText c (Just i) = show c <> " (" <> show i <> ")"
categoryText c Nothing = show c

faIcon :: forall e. String -> Markup e
faIcon = unsafeRawText <<< faIconRaw

categoryLink :: forall a t. (TagLike t) => t -> Maybe Int -> Markup a
categoryLink c mb = a ! href ("/cats/" <> ((show >>> limax) c)) $ text $ categoryText c mb

linkTo :: forall a e. {title :: String, published :: Maybe Date | e} -> Markup a
linkTo m = a ! href ("/posts/" <> limax m.title) $ text $ m.title <> (fromMaybe "" $ (formatDate >>> (\s -> " (" <> s <> ")")) <$> m.published)


