{-# LANGUAGE CPP #-}
#define __LOC__ (toMarkup( __FILE__ <> ":" <> show( __LINE__ :: Int ) ))
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib

import Text.Blaze
import Text.Blaze.Html5 hiding (main, head, span)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (id)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty
import System.IO

doLaTeXlogging :: Handle -> IO ()
doLaTeXlogging h = do
    let println = hPutStrLn h
    let printHtml = println . renderHtml
    let flush = hFlush h
    let getline = flush >> getLine
    println "<!DOCTYPE html>"
    println "<html>"
    printHtml katexStarterHead
    println "<body>"
    printHtml $ p $ __LOC__ <> "\\( \\displaystyle ~ \\frac{x}{y} \\)"
    s1 <- getline
    printHtml $ p ! A.id "s1" $ "1st hello world, " <> toMarkup s1 <> "?"
    printHtml $ mconcat . replicate 20 $ p "\\( \\displaystyle ~ \\frac{z}{w} \\)"
    s2 <- getline
    printHtml $ p ! A.id "s2" $ "2nd hello world, " <> toMarkup s2 <> "?"
    printHtml $ p $ __LOC__ <> "\\( \\displaystyle ~ \\frac{y}{z} \\)"
    printHtml $ mconcat . replicate 20 $ p "\\( \\displaystyle ~ \\frac{z}{x} \\)"
    println "</body>"
    println "</html>"

main :: IO ()
main = do
    -- with VSCode extension Live Server
    -- http://localhost:5500/outkatex.html
    -- http://localhost:5500/outmathjax.html
    -- (or, with any similar util web srv displaying fresh content on file update)
    putStrLn "using KaTeX ..."
    withFile "outkatex.html"   WriteMode doLaTeXlogging
    putStrLn "using MathJax ..."
    withFile "outmathjax.html" WriteMode doLaTeXlogging


