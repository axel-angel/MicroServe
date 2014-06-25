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

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ MainR GET
/#Text GetR GET
|]

instance Yesod HelloWorld where
    makeSessionBackend _ = return Nothing

{- Main pages -}

getMainR :: Handler Html
getMainR = listDir "."

getGetR :: Text -> Handler TypedContent
getGetR path = do
    isDir <- liftIO $ doesDirectoryExist $ unpack path
    if isDir
       then toTypedContent `fmap` listDir path
       else rawFile path

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
                    <a href=@{GetR $ fpath $ pack f}>
                        #{f}
    |]

main :: IO ()
main = warp 3000 HelloWorld
