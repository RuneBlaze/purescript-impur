module Impur.Types (PostMeta, Post, mkDate, PostRaw, PostContents, GMeta, GPost, categoryCount) where

import Prelude
import Data.Date (Date, canonicalDate)
import Data.Maybe (Maybe(..))
import Text.Smolder.Markup (Markup)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Enum (toEnum)
import Impur.Classes (class TagLike)
import Control.Monad.ST (modifySTRef, newSTRef, pureST, readSTRef)
import Data.Foldable (for_)

type PostMeta = forall a. (TagLike a) => {
    title :: String,
    published :: Maybe Date,
    category :: Maybe a
}

type PostRaw = forall e. Markup e

type PostContents = PostMeta -> PostRaw

type Post = forall a. PostMeta /\ (PostMeta -> Markup a)

type GMeta e = forall e. (TagLike e) => {category :: Maybe e, published :: Maybe Date, title :: String}

type GPost t m = forall t m. (TagLike t) => (GMeta t) /\ ((GMeta t) -> Markup m)

mkDate :: Int -> Int -> Int -> Maybe Date
mkDate y m d = do
    y' <- toEnum y
    m' <- toEnum m
    d' <- toEnum d
    pure $ canonicalDate y' m' d'

categoryCount :: forall t r. (TagLike t) => t -> Array (forall l. {category :: Maybe t | r} /\ ((GMeta t) -> Markup l)) -> Int
categoryCount cat psts = pureST do
    cnt <- newSTRef 0
    for_ psts $ \(m /\ _) ->
        modifySTRef cnt (\x -> x + if show <$> m.category == show <$> Just cat then 1 else 0)
    readSTRef cnt