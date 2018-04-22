module Impur.Site (static, precompile) where

import Control.Monad.Eff.Exception
import Control.Monad.Eff.Now
import Data.Tuple.Nested
import Node.Buffer
import Node.Encoding
import Node.FS
import Node.FS.Stats
import Node.FS.Sync
import Node.Path
import Prelude
import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes
import Text.Smolder.Markup

import CSS (pt)
import Control.Monad.Eff (Eff)
import Control.Plus (empty)
import Data.Foldable (for_)
import Node.Buffer (fromString) as B
import Node.FS.Sync (writeFile)
import Prelude as P
import Text.Smolder.Renderer.String (render)

copySync :: forall eff. FilePath -> FilePath -> Eff (buffer :: BUFFER, fs :: FS, exception :: EXCEPTION | eff) Unit
copySync lhs rhs = do
    f1 <- readFile lhs
    writeFile rhs f1

cleardirR :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit
cleardirR p = do
    files <- readdir p
    for_ files \n -> do
        let pt = (p <> "/" <> n)
        st <- stat (p <> "/" <> n)
        if isDirectory st then cleardirR pt else unlink pt
    rmdir p

rmdirF :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit
rmdirF p = do
    cleardirR p
    mkdir p

precompile :: forall eff. Eff (fs :: FS, buffer :: BUFFER, exception :: EXCEPTION | eff) Unit
precompile = do
    s <- exists "_site"
    when s (rmdirF "_site")
    when (not s) $ mkdir "_site"
    files <- readdir "static"
    let g st = ("static/" <> st) /\ ("_site/" <> st)
    let fns = P.map g files
    for_ fns \(a /\ b) -> copySync a b
    mkdir "_site/posts"
    mkdir "_site/cats"

prefixPath :: String -> String
prefixPath s = "_site/" <> s

static :: forall a eff. String -> Html a -> Eff (buffer :: BUFFER, fs :: FS, exception :: EXCEPTION | eff) Unit
static path html = do
    let contents = render html
    str <- B.fromString contents UTF8
    writeFile (prefixPath path <> ".html") str