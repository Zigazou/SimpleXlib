{-|
Module      : SimpleXlib
Description : Simplifies Xlib for use in Haskell
Copyright   : © 2016 Frédéric BISSON
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : alpha
Portability : POSIX

This library simplifies the use of Xlib within Haskell. It allows to clear the
code of repeated "display" and "window" parameters.
-}
module Graphics.X11.SimpleXlib.SimpleXlib
( WinState ( WinState, wsDisplay, wsRoot, wsWindow, wsScreen, wsScreenNumber
           , wsWmDeleteWindow , wsEventPtr , wsData
           )
-- * Event handling definition
, EventHandler
, EventHandlers
-- * Retrieving info from the current window state
, getData
, getDisplay
, getWindow
, getScreen
, getScreenNumber
, getEventPtr
-- * Stateful versions of some X functions
, createGC
, setForeground
, nextEvent
, getEvent
, fillRectangle
-- * Window creation
, createWindow
-- * Handler management
, listenEvents
, handlerExit
, handlerContinue
)
where

import qualified Graphics.X11.Types as X
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as X

import qualified Data.Map.Lazy as Map

import Control.Monad.Trans.State (evalStateT, StateT, get)
import Control.Monad.IO.Class (liftIO)

import Foreign.Ptr (nullPtr)

import Data.Bits ((.|.))

{-|
A Window state. This record keeps track of often used values like the display
or the window.

It also contains a pointer to an event structure so the user does not have to
handle it.
-}
data WinState a = WinState
    { wsDisplay        :: X.Display -- ^ the current display
    , wsRoot           :: X.Drawable -- ^ the root window
    , wsWindow         :: X.Drawable -- ^ the window
    , wsScreen         :: X.Screen -- ^ the current screen
    , wsScreenNumber   :: X.ScreenNumber -- ^ the current screen number
    , wsWmDeleteWindow :: X.Atom -- ^ the atom of delete window
    , wsEventPtr       :: X.XEventPtr -- ^ a pointer to an event
    , wsData           :: a -- ^ custom data
    } deriving (Show)

{-|
An event handler is a function which takes an event as input and returns
a state transformer of window’s state in IO monad, returning a boolean.

If the boolean is set to True, the loop handler will go on handling events.
Otherwise, it will stop handling events.
-}
type EventHandler a = X.Event -> StateT (WinState a) IO Bool

{-|
To simplify associations between event type and their handlers, event handlers
are given as a list of tuples of event type and event handler.

Behind the scene, the loop handler will convert this list to a Map, speeding
up the calling of the right handler when there are lots of handlers.
-}
type EventHandlers a = [(X.EventType, EventHandler a)]

{-|
Returns the custom data associated with the current window state.
-}
getData :: Monad m => StateT (WinState a) m a
getData = get >>= return . wsData

{-|
Returns the display set in the current window state.
-}
getDisplay :: Monad m => StateT (WinState a) m X.Display
getDisplay = get >>= return . wsDisplay

{-|
Returns the window set in the current window state.
-}
getWindow :: Monad m => StateT (WinState a) m X.Drawable
getWindow = get >>= return . wsWindow

{-|
Returns the screen set in the current window state.
-}
getScreen :: Monad m => StateT (WinState a) m X.Screen
getScreen = get >>= return . wsScreen

{-|
Returns the screen number set in the current window state.
-}
getScreenNumber :: Monad m => StateT (WinState a) m X.ScreenNumber
getScreenNumber = get >>= return . wsScreenNumber

{-|
Returns the event pointer set in the current window state.
-}
getEventPtr :: Monad m => StateT (WinState a) m X.XEventPtr
getEventPtr = get >>= return . wsEventPtr

{-|
Create a graphic context (GC) based on the current window state.
-}
createGC :: StateT (WinState a) IO X.GC
createGC = do
    display <- getDisplay
    window <- getWindow
    liftIO $ X.createGC display window

{-|
Set the foreground color of a graphic context (GC) based on the current window
state.
-}
setForeground :: X.GC -> X.Pixel -> StateT (WinState a) IO ()
setForeground gc px = do
    display <- getDisplay
    liftIO $ X.setForeground display gc px

{-|
Retrieve the next event to be handled. A call to this function is generally
followed by a call to the `getEvent` function.
-}
nextEvent :: StateT (WinState a) IO ()
nextEvent = do
    display <- getDisplay
    eventPtr <- getEventPtr
    liftIO $ X.nextEvent display eventPtr

{-|
Return the current event being handled.
-}
getEvent :: StateT (WinState a) IO X.Event
getEvent = do
    eventPtr <- getEventPtr
    liftIO $ X.getEvent eventPtr


{-|
Draw a filled rectangle according to a graphic context (GC) based on the
current window state.
-}
fillRectangle :: X.GC
              -> X.Position -> X.Position
              -> X.Dimension -> X.Dimension
              -> StateT (WinState a) IO ()
fillRectangle gc x y w h = do
    display <- getDisplay
    window <- getWindow
    liftIO $ X.fillRectangle display window gc x y w h

{-|
Create a simple window and returns the corresponding window state.
-}
createWindow :: X.Display
             -> X.Position -> X.Position
             -> X.Dimension -> X.Dimension
             -> a
             -> IO (WinState a)
createWindow display xPos yPos winWidth winHeight customData = do
    -- Get info from the default display
    let screen       = X.defaultScreenOfDisplay display
        screenNumber = X.screenNumberOfScreen screen
        whitePixel   = X.whitePixel display screenNumber
        blackPixel   = X.blackPixel display screenNumber

    -- Get the root window of the display
    root <- X.rootWindow display screenNumber

    -- Create a simple window on the display at root level
    window <- X.createSimpleWindow
                display
                root
                xPos yPos
                winWidth winHeight
                2
                blackPixel whitePixel

    -- Choose which events our window will handle
    X.selectInput display window (X.exposureMask .|. X.keyPressMask)

    -- Tells the window manager we will handle the close button
    wmdw <- X.internAtom display "WM_DELETE_WINDOW" False
    X.setWMProtocols display window [wmdw]

    -- Show the window
    X.mapWindow display window

    return $ WinState
            { wsDisplay        = display
            , wsRoot           = root
            , wsWindow         = window
            , wsScreen         = screen
            , wsScreenNumber   = screenNumber
            , wsWmDeleteWindow = wmdw
            , wsEventPtr       = nullPtr
            , wsData           = customData
            }

{-|
Empty handler forcing the loop to end.
-}
handlerExit :: EventHandler a
handlerExit _ = return False

{-|
Empty handler forcing the loop to continue.
-}
handlerContinue :: EventHandler a
handlerContinue _ = return True

{-|
Listen to events based on a window state and calls the corresponding event
handler.

This function will infinitely handle events until one of the handlers returns
False.
-}
listenEvents :: WinState a -> EventHandlers a -> IO ()
listenEvents winState eventHandlers =
    X.allocaXEvent $ \eventPtr -> do
        let winState' = winState { wsEventPtr = eventPtr }
        evalStateT loopHandler winState' >> return ()

    where
        eventHandlers' = Map.fromList eventHandlers
        loopHandler = do
            event <- nextEvent >> getEvent
            let eventType = X.ev_event_type event

            continue <- case Map.lookup eventType eventHandlers' of
                 Nothing -> return True
                 Just handler -> handler event

            if continue
               then loopHandler
               else return False
