{-# LANGUAGE OverloadedStrings #-}
module Views.Site where

import           Data.Monoid
import qualified Data.Text                   as T
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

data SiteView
   = SiteView
   { svUser :: Maybe T.Text
   }

siteView :: SiteView -> H.Html -> H.Html
siteView sv body =
    H.html $
    do H.head $
        do H.title "Revenue App"
           H.meta ! A.charset "utf-8"
           H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
           H.link ! A.href "/css/bootstrap.min.css" ! A.rel "stylesheet"
           H.link ! A.href "/css/blog.css" ! A.rel "stylesheet"
       H.body $
        do H.div ! A.class_ "blog-masthead" $
            H.div ! A.class_ "container" $
             H.nav ! A.class_ "blog-nav" $
              do H.a ! A.class_ "blog-nav-item" ! A.href "/" $ "Home"
                 case svUser sv of
                   Nothing ->
                       H.a ! A.class_ "blog-nav-item" ! A.href "/login" $ "Login"
                   Just user ->
                       H.a ! A.class_ "blog-nav-item" ! A.href "/logout" $ H.toHtml ("Logout " <> user)
           H.div ! A.class_ "container" $ H.div ! A.class_ "blog-main" $ body
           H.div ! A.class_ "blog-footer" $
            do H.p $
                do H.span "Blog template built for "
                   H.a ! A.href "http://getbootstrap.com" $ "Bootstrap"
                   H.span " by "
                   H.a ! A.href "https://twitter.com/mdo" $ "@mdo"
               H.p $
                H.a ! A.href "#" $ "Back to top"
           H.script ! A.href "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js" $ mempty
           H.script ! A.href "/js/bootstrap.min.js" $ mempty
