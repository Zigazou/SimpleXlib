module Main where

import qualified Graphics.X11.Types as X
import qualified Graphics.X11.Xlib as X

import Graphics.X11.SimpleXlib.SimpleXlib

-- Dimensions of the window and the squares
winWidth, winHeight, sqWidth, sqHeight :: Num a => a
winWidth  = 640
winHeight = 500
sqWidth   = 20
sqHeight  = 20

-- List of colors used for the squares
colorNames :: [String]
colorNames = 
    [ "black", "red", "green", "yellow", "blue", "magenta", "cyan", "white" ]

-- Get actual colors for a display based on a list of human colors
getColors :: X.Display -> [String] -> IO [X.Color]
getColors display names = mapM getColor names
    where cmap = X.defaultColormap display (X.defaultScreen display)
          getColor name = X.allocNamedColor display cmap name >>= return . snd

-- Handle the events
eventHandlers :: EventHandlers [X.Color]
eventHandlers =
    [ (X.expose       , exposeHandler)
    , (X.keyPress     , handlerExit)
    , (X.clientMessage, handlerExit)
    ]

-- Handles the expose event (draw a grid of squares in different colors)
exposeHandler :: EventHandler [X.Color]
exposeHandler _ = do
    let poss = [(x, y) | y <- [0, sqHeight .. winHeight]
                       , x <- [0, sqWidth .. winWidth]
               ]
    colors <- getData
    gc <- createGC

    mapM_ (drawRectangle gc) (zip poss (cycle colors))

    return True
    
    where drawRectangle gc ((x, y), color) = do
                setForeground gc (X.color_pixel color)
                fillRectangle gc x y sqWidth sqHeight

main :: IO ()
main = do
    -- Get the default display
    display <- X.openDisplay ""

    -- Retrieves actual colors for the display
    colors <- getColors display colorNames

    winState <- createWindow display 0 0 winWidth winHeight colors

    listenEvents winState eventHandlers
