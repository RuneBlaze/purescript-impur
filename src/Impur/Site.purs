module Impur.Site (static, precompile) where


import Prelude
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Stats (isDirectory)
import Node.FS.Sync (exists, mkdir, readFile, readdir, rmdir, stat, unlink, writeFile)
import Node.Path (FilePath)
import Text.Smolder.HTML (Html)
import Control.Monad.Eff (Eff)
import Data.Foldable (for_)
import Node.Buffer (fromString) as B
import Text.Smolder.Renderer.String (render)

copySync :: forall eff. FilePath -> FilePath -> Eff (buffer :: BUFFER, fs :: FS, exception :: EXCEPTION | eff) Unit
copySync lhs rhs = do
    f1 <- readFile lhs
    writeFile rhs f1


cleardirRCached :: forall eff. FilePath -> FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit
cleardirRCached p r = do
    files <- readdir p
    for_ files \n -> do
        let pt = (p <> "/" <> n)
        let rt = (r <> "/" <> n)
        st <- stat (p <> "/" <> n)
        if isDirectory st then cleardirRCached pt rt else unlessM (exists rt) $ unlink pt
    unlessM (exists r) $ rmdir p

cleardirR :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit
cleardirR p = do
    files <- readdir p
    for_ files \n -> do
        let pt = (p <> "/" <> n)
        st <- stat (p <> "/" <> n)
        if isDirectory st then cleardirR pt else unlink pt
    rmdir p

copySyncR :: forall eff. FilePath -> FilePath -> Eff (buffer :: BUFFER, fs :: FS, exception :: EXCEPTION | eff) Unit
copySyncR lhs rhs = do
    files <- readdir lhs
    for_ files \n -> do
        let p = (lhs <> "/" <> n)
        let r = (rhs <> "/" <> n)
        st <- stat p
        if isDirectory st
            then do
                unlessM (exists r) $ mkdir (rhs <> "/" <> n)
                copySyncR (lhs <> "/" <> n) (rhs <> "/" <> n)
            else unlessM (exists r) $ copySync (lhs <> "/" <> n) (rhs <> "/" <> n)

rmdirF :: forall eff. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | eff) Unit
rmdirF p = do
    cleardirR p
    mkdir p

precompile :: forall eff. Eff (fs :: FS, buffer :: BUFFER, exception :: EXCEPTION | eff) Unit
precompile = do
    s <- exists "_site"
    when s $ cleardirRCached "_site" "static"
    when (not s) $ mkdir "_site"
    files <- readdir "static"
    copySyncR "static" "_site"
    mkdir "_site/posts"
    mkdir "_site/cats"

prefixPath :: String -> String
prefixPath s = "_site/" <> s

static :: forall a eff. String -> Html a -> Eff (buffer :: BUFFER, fs :: FS, exception :: EXCEPTION | eff) Unit
static path html = do
    let contents = render html
    str <- B.fromString contents UTF8
    writeFile (prefixPath path <> ".html") str