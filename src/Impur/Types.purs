module Impur.Types where

import Prelude
import Data.Date (Date, canonicalDate)
import Data.Maybe (Maybe)
import Text.Smolder.Markup (Markup)
import Data.Tuple.Nested (type (/\))
import Data.Enum (toEnum)
import Impur.Conf (Category)

type PostMeta = {
    title :: String,
    published :: Maybe Date,
    category :: Maybe Category
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