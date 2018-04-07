module Test.Main where

import Prelude
import Limax (limax)
import Hljs (highlight)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
    log $ highlight "ruby" "puts 1"
    log $ limax "我要回家"