{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
import Yesod.Core
import Yesod.Form

import Prelude hiding (head, readFile)
import System.Directory (getDirectoryContents, doesDirectoryExist
                        , doesFileExist)
import Network.Mime (defaultMimeLookup)
import Data.Text (Text, pack, unpack)
import Data.ByteString.Lazy (readFile)
import Data.Monoid ((<>))
import System.FilePath ((</>))
import Network.HTTP.Types
import Data.List (sort)
import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Text.Regex.Posix
import Data.String (IsString(..))
import Control.Monad (when, forM_)

data MicroServe = MicroServe
newtype UnsafePath = UnsafePath Text
    deriving (Eq, Show, Read, PathPiece, IsString)

mkYesod "MicroServe" [parseRoutes|
/            MainR GET POST
/#UnsafePath GoR   GET POST
|]

instance Yesod MicroServe where
    makeSessionBackend _ = return Nothing
    maximumContentLength _ _ = Just $ 10 * 1024 * 1024 * 1024 -- 10 Go
instance RenderMessage MicroServe FormMessage where
    renderMessage _ _ = defaultFormMessage
type Form a = Html -> MForm (HandlerT MicroServe IO) (FormResult a, Widget)

{- Main pages -}

getMainR :: Handler Html
getMainR = listDir ""

postMainR :: Handler Text
postMainR = postIn ""

getGoR :: UnsafePath -> Handler TypedContent
getGoR upath = do
    path <- getPathSafe upath
    let strPath = unpack path
    isDir <- liftIO $ doesDirectoryExist strPath
    isFile <- liftIO $ doesFileExist strPath
    case (isDir, isFile) of
        (True, False) -> toTypedContent <$> listDir path
        (False, True) -> rawFile path
        _ -> notFound

postGoR :: UnsafePath -> Handler Text
postGoR path = postIn path

{- Utils -}

rawFile :: Text -> Handler TypedContent
rawFile path = do
    let cType = defaultMimeLookup path
    lbytes <- liftIO $ readFile $ unpack path
    return $ TypedContent cType $ toContent lbytes

listDir :: Text -> Handler Html
listDir path = do
    let (vpath, prefix) = if path == "" then (".", "") else (path, path <> "/")
        filterFiles = filter $ isPathSafe . pack -- hide unsafe
    fs <- filterFiles <$> (liftIO $ getDirectoryContents $ unpack vpath)
    let fpath f = prefix <> f

    defaultLayout [whamlet|
        <h1>
            Listing of #{vpath}

        <ul>
            $forall f <- sort fs
                <li>
                    <a href=@{GoR $ UnsafePath $ fpath $ pack f}>
                        #{f}

        <h2>
            Upload here
        <form method=POST enctype="multipart/form-data">
            <input type=file name=files multiple />
            <button type=submit>Upload
    |]

postIn :: UnsafePath -> Handler Text
postIn upath = do
    path <- getPathSafe upath
    exists <- (path == "" ||) <$> (liftIO $ doesDirectoryExist $ unpack path)
    when (not exists) $ notFound

    files <- lookupFiles "files"
    case files of
         [] -> sendResponseStatus status400 ("Invalid input" :: Text)
         xs -> do
             forM_ xs $ saveUpload path
             redirect $ GoR upath

saveUpload :: Text -> FileInfo -> Handler Text
saveUpload path file = do
    let fullPath = unpack path </> unpack (fileName file)
    liftIO $ fileMove file fullPath
    return $ pack fullPath

isPathSafe :: Text -> Bool
isPathSafe path = not $ unpack path =~ pattern
    where pattern = "(^|/)\\." :: String

getPathSafe :: UnsafePath -> Handler Text
getPathSafe (UnsafePath path) = if safe then return path else notFound
    where safe = isPathSafe path

{- Main -}
main :: IO ()
main = do
    args <- getArgs
    let port = if length args > 0 then read (args !! 0) else 3000
    warp port MicroServe
