module Katex (renderToString, katex) where

import Prelude (($))
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)

foreign import renderToStringNullable :: String -> Nullable String

renderToString :: String -> Maybe String
renderToString s = toMaybe $ renderToStringNullable s

katex :: String -> Maybe String
katex = renderToString