{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Model
import           Queries
import           Utils
import           Views.Site

import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Data.Validator
import           Database.Persist.Sqlite       hiding (get)
import           GHC.Generics
import           Network.Wai.Middleware.Static
import qualified Text.Blaze.Bootstrap          as H
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5              as H
import qualified Text.Digestive                as F
import qualified Text.Digestive.Bootstrap      as F
import           Web.Spock
import qualified Web.Spock.Digestive           as F
import qualified Web.Spock.Rest                as R


type Username = T.Text
type Session = Maybe Username

main :: IO ()
main =
    do pool <- runNoLoggingT $ createSqlitePool "database.db" 5
       runNoLoggingT $ runSqlPool (runMigration migrateModel) pool
       launchApp pool

launchApp :: ConnectionPool -> IO ()
launchApp p =
    runSpock 3000 $ spock (defaultSpockCfg Nothing (PCPool p) ()) $
    do middleware (staticPolicy (addBase "static"))
       get "/" $ redirect "/login"
       get "logout" $ writeSession Nothing >> redirect "/"
       getpost "login" loginAction
       prehook checkSession $
            do get "member-area" memberArea
               subcomponent "api" $
                    R.post (R.JSON R.:~> R.JSON R.:|: R.CtNull) ("item" <//> "create") createItem

createItem :: Item -> SpockAction SqlBackend Session st CreateItemResult
createItem item =
    case runValidator validateItem item of
      Left errMsg -> return (CreateItemFailed errMsg)
      Right checkedItem -> CreateItemOkay <$> runSQL (insert checkedItem)

data CreateItemResult
   = CreateItemOkay ItemId
   | CreateItemFailed T.Text
    deriving (Show, Eq, Generic, ToJSON)

validateItem :: ValidationRule T.Text Item
validateItem i =
    do name <-
           lengthBetween 5 10 "The length of the item name must be between 5 and 10" (itemName i)
       price <-
           conformsPred (>=0) "The price must be larger or equal to zero" (itemPrice i)
       revenue <-
           conformsPred (\r -> r >= 0 && r <= price) "The revenue must be smaller than the price" (itemRevenue i)
       return $ Item name price revenue

checkSession :: SpockActionCtx ctx SqlBackend Session st ()
checkSession =
    do sess <- readSession
       when (isNothing sess) $ redirect "/login"

memberArea :: SpockAction SqlBackend Session st ()
memberArea =
    do customers <- runSQL $ selectList [] []
       totalRev <- runSQL $ (fromMaybe 0 . join . listToMaybe) <$> getTotalRevenue
       site $
            do H.h1 "Member's Club!"
               H.h2 $ H.toHtml $ "Total revenue is: " <> show totalRev <> " Euros."
               H.h2 "Customers"
               H.ul $
                    forM_ customers $ \customer ->
                    H.li $ H.toHtml (customerName $ entityVal customer)

loginAction :: SpockAction conn Session st ()
loginAction =
    do let formView = F.renderForm loginFormSpec
       f <- F.runForm "loginForm" loginForm
       case f of
         (view, Nothing) ->
             site $ formView view
         (view, Just loginReq) ->
             if lrUser loginReq == "admin" && lrPassword loginReq == "admin1"
             then do sessionRegenerateId
                     writeSession (Just $ lrUser loginReq)
                     redirect "/member-area"
             else site $
                  do H.alertBox H.BootAlertDanger "Sorry, login failed. Try again."
                     formView view

site :: H.Html -> SpockAction conn Session st ()
site ct =
    do sess <- readSession
       let sv = SiteView sess
       blaze $ siteView sv ct

blaze :: MonadIO m => H.Html -> ActionT m ()
blaze = html . TL.toStrict . renderHtml

data LoginRequest
   = LoginRequest
   { lrUser     :: T.Text
   , lrPassword :: T.Text
   } deriving (Show)

minMaxLen :: (Int, Int) -> T.Text -> F.Result H.Html T.Text
minMaxLen (minLen, maxLen) t =
   if len >= minLen && len <= maxLen
       then F.Success stripped
       else F.Error $ H.toHtml $
            "Must be longer than " ++ show minLen
            ++ " and shorter than "
            ++ show maxLen ++ " characters"
   where
     stripped = T.strip t
     len = T.length stripped

loginForm :: Monad m => F.Form H.Html m LoginRequest
loginForm =
   LoginRequest
   <$> "name" F..: F.validate (minMaxLen (3, 12)) (F.text Nothing)
   <*> "password" F..: F.validate (minMaxLen (6, 40)) (F.text Nothing)

loginFormSpec :: F.FormMeta
loginFormSpec =
   F.FormMeta
   { F.fm_method = POST
   , F.fm_target = "/login"
   , F.fm_elements =
       [ F.FormElement "name" (Just "Username") F.InputText
       , F.FormElement "password" (Just "Password") F.InputPassword
       ]
   , F.fm_submitText = "Login"
   }
