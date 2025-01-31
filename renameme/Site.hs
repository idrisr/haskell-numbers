{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll
import Text.Pandoc.Options

main :: IO ()
main = hakyll $ do
    match "*md" $ do
        route $ setExtension "html"
        compile $ do
            let writerOptions = def{writerTableOfContents = True, writerTOCDepth = 4}
            let readerOptions = def{readerStandalone = True}
            pandocCompilerWith readerOptions writerOptions
                >>= loadAndApplyTemplate "templates/default.html" defaultContext

    match "templates/*" $ compile templateBodyCompiler

    match "styles.css" $ do
        route idRoute
        compile compressCssCompiler
