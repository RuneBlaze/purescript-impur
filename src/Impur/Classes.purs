module Impur.Classes (class TagLike) where
import Prelude
    
class (Show a, Eq a) <= TagLike a