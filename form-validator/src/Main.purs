module Main where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Web.DOM.Element as Element
import Web.DOM.Node (textContent, setTextContent, nodeValue)
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.Event.Event (EventType, Event, target, preventDefault)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLInputElement (HTMLInputElement, fromEventTarget, value)
import Web.HTML.HTMLDocument as Document
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent.EventTypes as METypes

main :: Effect Unit
main = do
    win <- window
    doc <- document win
    let docAsParent = Document.toParentNode doc

    mbForm <- querySelector (QuerySelector "#form") docAsParent
    mbUserName <- querySelector (QuerySelector "#username") docAsParent
    mbEmail <- querySelector (QuerySelector "#email") docAsParent
    mbPassword <- querySelector (QuerySelector "#password") docAsParent
    mbPassword2 <- querySelector (QuerySelector "#password2") docAsParent

    mbButtonMain <- querySelector (QuerySelector "#mainButton") docAsParent

    case mbForm, mbUserName, mbEmail, mbPassword, mbPassword2, mbButtonMain of
        Just form, Just userName, Just email, Just password, Just password2,
        Just bMain -> do
          let
            buttonMain    = Element.toEventTarget bMain

          removeListener <- addBetterListener METypes.click false buttonMain \e -> do
             preventDefault e
             -- eventValue e >>= log
             inputValue (fromEventTarget (Element.toEventTarget userName)) >>=
                 log

          log "Done"

        _, _, _, _, _, _ -> do
          log $ "Could not get all buttons."

    log "Test"

addBetterListener
  :: forall a
   . EventType -> Boolean -> EventTarget -> (Event -> Effect a) -> Effect (Effect Unit)
addBetterListener type_ useCaptureRatherThanBubble target listener = do
  evListener <- eventListener listener
  addEventListener type_ evListener useCaptureRatherThanBubble target
  pure $
    removeEventListener type_ evListener useCaptureRatherThanBubble target

eventValue :: Event -> Effect String
eventValue evt = targetValue (target evt)

targetValue :: Maybe EventTarget -> Effect String
targetValue (Just et) = inputValue (fromEventTarget et)
targetValue _ = pure ""

inputValue :: Maybe HTMLInputElement -> Effect String
inputValue (Just el) = value el
inputValue _ = pure ""

maybeText :: Maybe Element.Element -> Effect String
maybeText (Just el) = textContent  (Element.toNode el)
maybeText _ = pure ""
