{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Html.Logger
    ( katexStarterHead
    , mathjaxStarterHead
    , tftree, tftreeWith
    , tfnc, tfnc_, tfncWith, tfncWith_
    ) where

import Text.Blaze.Html5 hiding (head, div, style)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (id)
import qualified Text.Blaze.Html5.Attributes as A

scrollToBottomOnLoad :: Html
scrollToBottomOnLoad = script "\
\const scrollToBottom = () => { \
\    const scrollingElement = (document.scrollingElement || document.body);\
\    scrollingElement.scrollTop = scrollingElement.scrollHeight;\
\};\
\document.querySelector('body').onload = setTimeout(scrollToBottom, 250);\
\"

-- https://dumptyd.github.io/treeflex/ accessed 2022-12-27
treeFlex :: Html
treeFlex = link
    ! rel "stylesheet"
    ! href "https://unpkg.com/treeflex/dist/css/treeflex.css"

tftree :: [Html] -> Html
tftree = tftreeWith []

tftreeWith :: [Attribute] -> [Html] -> Html
tftreeWith as = foldl (!) H.div (class_ "tf-tree" : as) . ul . mconcat


tfncWith :: [Attribute] -> Html -> [Html] -> Html
tfncWith as v [] = li $ foldl (!) H.span (class_ "tf-nc" : as) v
tfncWith as v ts = li $ do
    foldl (!) H.span (class_ "tf-nc" : as) v
    ul $ mconcat ts

tfncWith_ :: [Attribute] -> Html -> Html
tfncWith_ as v = tfncWith as v []

tfnc :: Html -> [Html] -> Html
tfnc v [] = li $ H.span ! class_ "tf-nc" $ v
tfnc v ts = li $ do H.span ! class_ "tf-nc" $ v
                    ul $ mconcat ts

tfnc_ :: Html -> Html
tfnc_ v = tfnc v []

commonHeaderContents :: Html
commonHeaderContents = do 
    scrollToBottomOnLoad
    treeFlex

-- https://katex.org/docs/browser.html#starter-template accessed 2022-12-26
katexStarterHead :: Html
katexStarterHead = H.head $ do
    commonHeaderContents
    link
        ! rel "stylesheet"
        ! href "https://cdn.jsdelivr.net/npm/katex@0.16.4/dist/katex.min.css"
        ! integrity "sha384-vKruj+a13U8yHIkAyGgK1J3ArTLzrFGBbBc0tDp4ad/EyewESeXE/Iv67Aj8gKZ0"
        ! crossorigin "anonymous"
    -- The loading of KaTeX is deferred to speed up page rendering
    script
        ! defer "defer"
        ! src "https://cdn.jsdelivr.net/npm/katex@0.16.4/dist/katex.min.js"
        ! integrity "sha384-PwRUT/YqbnEjkZO0zZxNqcxACrXe+j766U2amXcgMg5457rve2Y7I6ZJSm2A0mS4"
        ! crossorigin "anonymous"
        $ ""
    -- To automatically render math in text elements, include the auto-render extension
    script
        ! defer "defer"
        ! src "https://cdn.jsdelivr.net/npm/katex@0.16.4/dist/contrib/auto-render.min.js"
        ! integrity "sha384-+VBxd3r6XgURycqtZ117nYw44OOcIax56Z4dCRWbxyPt0Koah1uHoK0o4+/RRE05"
        ! crossorigin "anonymous"
        ! onload "renderMathInElement(document.body);"
        $ ""

-- attributes not predefined in blaze-html
integrity :: AttributeValue -> Attribute
integrity = customAttribute "integrity"
crossorigin :: AttributeValue -> Attribute
crossorigin = customAttribute "crossorigin"

-- https://www.mathjax.org/#gettingstarted accessed 2022-12-27
mathjaxStarterHead :: Html
mathjaxStarterHead = H.head $ do
    commonHeaderContents
    script
        ! src "https://polyfill.io/v3/polyfill.min.js?features=es6"
        ! crossorigin "anonymous"
        $ ""
    -- To automatically render math in text elements, include the auto-render extension
    script
        ! async "async"
        ! A.id "MathJax-script"
        ! src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
        ! crossorigin "anonymous"
        $ ""
