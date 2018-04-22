module Impur.Katex (renderToString, katex) where

import Prelude (($))
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)

foreign import renderToStringNullable :: String -> Boolean -> Nullable String

renderToString :: String -> Boolean -> Maybe String
renderToString s b = toMaybe $ renderToStringNullable s b

katex :: String -> Boolean -> Maybe String
katex = renderToString