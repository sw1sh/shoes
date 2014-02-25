{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TypeFamilies, QuasiQuotes,
  TemplateHaskell, MultiParamTypeClasses, GADTs, FlexibleContexts #-}

import ClassyPrelude
import Yesod
import Yesod.Default.Config (DefaultEnv (..), withYamlEnvironment)
import Yesod.Static
import Database.Persist.Sqlite (withSqlitePool, runSqlPool, runMigration, ConnectionPool (..), SqlPersistT)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Applicative ((<$>), (<*>))
import Data.Conduit
import Data.Conduit.Base64
import Data.Conduit.Binary

data App = App
    { 
      pool :: ConnectionPool,
      getStatic :: Static
    }

instance Yesod App 

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB act = do
      App pool _ <- getYesod
      runSqlPool act pool

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Shoe
  desc Text
  color Text
  size Int
  deriving Show
|]

data ShoeJSON = ShoeJSON Shoe LByteString
  deriving Show

instance FromJSON ShoeJSON where
  parseJSON (Object v) = do
   shoeData <- Shoe <$> v .: "description" <*> v .: "color" <*> v .: "size"
   shoePicture <- v .: "photo"
   return $ ShoeJSON shoeData (fromString shoePicture)

mkYesod "App" [parseRoutes|
/             HomeR     GET
/new          NewShoeR  POST
/shoe/#ShoeId ShoeR     GET
/static       StaticR   Static getStatic
|]

{-===========================================================================-}
{-                                  HANDLERS                                 -}
{-===========================================================================-}

getHomeR :: Handler Html
getHomeR = do
  shoes <- runDB $ selectList [] [Asc ShoeDesc]
  defaultLayout $ do
    setTitle "Shoe inventory"
    toWidget [whamlet|
      $forall Entity shoeId shoe <- shoes
        <ul>
          <a href=@{ShoeR shoeId}>#{shoeDesc shoe}
          <div style="color:#{shoeColor shoe}">color
          <div>Size: #{shoeSize shoe}
    |]

postNewShoeR :: Handler Html
postNewShoeR = do
  ShoeJSON shoe photo <- parseJsonBody_
  shoeId <- runDB $ insert shoe
  let PersistInt64 k = unKey shoeId
  runResourceT $ sourceLbs photo $= decode $$ sinkFile ("static/img/" ++ show k)
  redirect $ ShoeR shoeId

getShoeR :: ShoeId -> Handler Html
getShoeR shoeId = do
  shoe <- runDB $ get404 shoeId
  let PersistInt64 k = unKey shoeId
  defaultLayout
    [whamlet|
      <div>#{shoeDesc shoe}
      <div style="color:#{shoeColor shoe}">color
      <div>Size: #{shoeSize shoe}
      <img src="/static/img/#{show k}">
    |]

{-===========================================================================-}
{-                                 MAIN                                      -}
{-===========================================================================-}

main = withSqlitePool "shoes.sqlite" 10 $ \pool -> do
  runResourceT $ runStderrLoggingT $ flip runSqlPool pool $ do
    runMigration migrateAll
  static <- static "static"
  warp 3000 $ App pool static
