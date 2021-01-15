module Main where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log)
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument as Document
import Web.HTML.Window (document)

main :: Effect Unit
main = do
    win <- window
    doc <- document win
    let docAsParent = Document.toParentNode doc

    mbDiv <- querySelector (QuerySelector ".test") docAsParent
    for_ mbDiv \e -> do
      log "Found `div` element with class"

    log "Test"
