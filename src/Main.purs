module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Foldable (for_)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Tuple.Nested (type (/\), (/\))
import Node.Express.Types (EXPRESS)
import Impur.Index (index, posts) as I
import Impur.Site (static, precompile) as S
import Node.Buffer (BUFFER)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Impur.Limax (limax)

main :: forall e. Eff (console :: CONSOLE, buffer :: BUFFER, express :: EXPRESS, fs :: FS, exception :: EXCEPTION | e) Unit
main = do
    S.precompile
    S.static "index" I.index
    for_ I.posts \(meta /\ contents) -> let markup = contents meta in
        S.static ("posts/" <> limax meta.title) markup