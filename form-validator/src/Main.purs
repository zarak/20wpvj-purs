module Main where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V, invalid)
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Web.DOM.Element as Element
import Web.DOM.Node (textContent, setTextContent, nodeValue, parentElement)
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.Event.Event (EventType, Event, target, preventDefault)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLInputElement (HTMLInputElement, fromEventTarget, value)
import Web.HTML.HTMLDocument as Document
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent.EventTypes as METypes

type Errors = Array String

type Registration
    = { username :: String
      , email :: String
      , password :: String
      , password2 :: String
      }

main :: Effect Unit
main = do
    win <- window
    doc <- document win
    let docAsParent = Document.toParentNode doc

    mbForm <- querySelector (QuerySelector "#form") docAsParent
    mbUsername <- querySelector (QuerySelector "#username") docAsParent
    mbEmail <- querySelector (QuerySelector "#email") docAsParent
    mbPassword <- querySelector (QuerySelector "#password") docAsParent
    mbPassword2 <- querySelector (QuerySelector "#password2") docAsParent

    mbButtonMain <- querySelector (QuerySelector "#mainButton") docAsParent

    case mbForm, mbUsername, mbEmail, mbPassword, mbPassword2, mbButtonMain of
        Just form, Just username, Just email, Just password, Just password2,
        Just bMain -> do
          let
            buttonMain    = Element.toEventTarget bMain

          removeListener <- addBetterListener METypes.click false buttonMain \e -> do
             preventDefault e
             -- eventValue e >>= log
             usernameInput <- inputValue (fromEventTarget (Element.toEventTarget username))
             emailInput <- inputValue (fromEventTarget (Element.toEventTarget email))
             passwordInput <- inputValue (fromEventTarget (Element.toEventTarget password))
             password2Input <- inputValue (fromEventTarget (Element.toEventTarget password2))

             if usernameInput == ""
                 then showError username "Username is required"
                 else showSuccess username

             if emailInput == ""
                 then showError email "Email is required"
                 else showSuccess email

             if passwordInput == ""
                 then showError password "Password is required"
                 else showSuccess password

             if password2Input == ""
                 then showError password2 "Password is required"
                 else showSuccess password2

             validateRegistration { username = usernameInput 
                                  , email = emailInput 
                                  , password = passwordInput 
                                  , password2 = password2Input 
                                  }
          log "Done"

        _, _, _, _, _, _ -> do
          log $ "Could not get all buttons."

    log "Test"

showError :: Element.Element -> String -> Effect Unit
showError input message = do
    formControl <- parentElement $ Element.toNode input
    case formControl of
         Nothing -> log $ "Element not found"
         Just fc -> do
            liftEffect $ Element.setClassName "form-control error" fc
            smallMessage fc message

showSuccess :: Element.Element -> Effect Unit
showSuccess input = do
    formControl <- parentElement $ Element.toNode input
    case formControl of
         Nothing -> log $ "Element not found"
         Just fc -> Element.setClassName "form-control success" fc
         
smallMessage :: Element.Element -> String -> Effect Unit
smallMessage el msg = do
    let node = Element.toParentNode el
    mbSmall <- querySelector (QuerySelector "small") node
    case mbSmall of
         Nothing -> log "NOthing"
         Just sm -> setTextContent msg (Element.toNode sm)

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


nonEmpty :: String -> String -> V Errors String
nonEmpty field "" = invalid [ "Field '" <> field <> "' cannot be empty" ]
nonEmpty _ value = pure value


registration :: String -> String -> String -> String -> Registration
registration username email password password2 = 
    { username, email, password, password2 }

validateRegistration :: Registration -> V Errors Registration
validateRegistration r =
    registration <$> nonEmpty "username" r.username
                 <*> nonEmpty "email" r.email
                 <*> nonEmpty "password" r.password
                 <*> nonEmpty "password2" r.password2
