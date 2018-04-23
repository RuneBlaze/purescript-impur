module Impur.Types (PostMeta, Post, mkDate, PostRaw, PostContents) where

import Prelude
import Data.Date (Date, canonicalDate)
import Data.Maybe (Maybe(..))
import Text.Smolder.Markup (Markup)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Enum (toEnum)
import Impur.Conf (Category(..))
import Impur.Classes (class TagLike)

type PostMeta = forall a. (TagLike a) => {
    title :: String,
    published :: Maybe Date,
    category :: Maybe a
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