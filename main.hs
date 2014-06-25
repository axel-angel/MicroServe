{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
import Yesod
import Prelude hiding (head, readFile)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Network.Mime (defaultMimeLookup)
import Data.Text
import Data.ByteString.Lazy (readFile)
import Data.Monoid ((<>))
import System.FilePath ((</>))
import Network.HTTP.Types

data MicroServe = MicroServe

mkYesod "MicroServe" [parseRoutes|
/      MainR GET POST
/#Text GoR   GET POST
|]

instance Yesod MicroServe where
    makeSessionBackend _ = return Nothing
instance RenderMessage MicroServe FormMessage where
    renderMessage _ _ = defaultFormMessage

{- Main pages -}

getMainR :: Handler Html
getMainR = listDir "."

postMainR :: Handler Text
postMainR = postIn "."

getGoR :: Text -> Handler TypedContent
getGoR path = do
    isDir <- liftIO $ doesDirectoryExist $ unpack path
    if isDir
       then toTypedContent `fmap` listDir path
       else rawFile path

postGoR :: Text -> Handler Text
postGoR path = postIn path

{- Utils -}

rawFile :: Text -> Handler TypedContent
rawFile path = do
    let cType = defaultMimeLookup path
    lbytes <- liftIO $ readFile $ unpack path
    return $ TypedContent cType $ toContent lbytes

listDir :: Text -> Handler Html
listDir path = do
    fs <- liftIO $ getDirectoryContents $ unpack path
    let fpath f = path <> "/" <> f
    defaultLayout [whamlet|
        <h1>
            Listing of #{path}

        <ul>
            $forall f <- fs
                <li>
                    <a href=@{GoR $ fpath $ pack f}>
                        #{f}
    |]

postIn :: Text -> Handler Text
postIn path = do
    res <- runInputPostResult $ ireq fileField "file"
    ur <- getUrlRender
    case res of
         FormSuccess f -> do
             path <- saveUpload path f
             sendResponseStatus status200 $ (ur $ GoR path) <> "\n"
         _ -> do
             sendResponseStatus status400 ("Invalid input" :: Text)

saveUpload :: Text -> FileInfo -> Handler Text
saveUpload path file = do
    let fullPath = unpack path </> unpack (fileName file)
    liftIO $ fileMove file fullPath
    return $ pack fullPath

{- Main -}
main :: IO ()
main = warp 3000 MicroServe
