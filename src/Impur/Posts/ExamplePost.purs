module Impur.Posts.ExamplePost (post) where

import Prelude (($), class Semiring, (+))
import Impur.Types (PostMeta, PostRaw, PostContents, Post, mkDate)
import Impur.Tmpl (blogTemplate)
import Text.Smolder.Markup
import Data.Date
import Data.Tuple.Nested ((/\), type (/\))
import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes
import Impur.Conf (Category(..))
import Data.Maybe (Maybe(..))
import Impur.Classes (class TagLike)


postMeta :: {
    title :: String,
    published :: Maybe Date,
    category :: Maybe Category
}
postMeta = {title: "Example Post", published: mkDate 2018 4 2, category: Just Programming }

contents :: forall t e. (TagLike t) => {
    title :: String,
    published :: Maybe Date,
    category :: Maybe t | e
} -> forall a. Markup a
contents = blogTemplate $ do
    p $ text "this is my first paragraph"


post :: forall t13.                      
                   { title :: String           
                   , published :: Maybe Date   
                   , category :: Maybe Category
                   } /\                 
                   ({ title :: String          
                    , published :: Maybe Date  
                    , category :: Maybe Category    
                    | t13                      
                    }                          
                    -> forall e. Markup e
                   )
post = postMeta /\ contents