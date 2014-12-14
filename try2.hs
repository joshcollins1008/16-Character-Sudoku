{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import Control.Concurrent.STM
import Control.Concurrent

import           Graphics.Blank

main :: IO ()
main = do
  state_var <- atomically $ newTVar startState
  blankCanvas 3000 { events = ["mousedown"] } $ \ context -> do
        forkIO $ viewer context state_var
        control_loop context state_var

data NUM = None | Zero | One | Two 
            | Three | Four | Five | Six 
          deriving (Eq,Ord,Show) 

nextNum :: NUM -> NUM
nextNum None = Zero
nextNum Zero = One
nextNum One = Two
nextNum Two = Three
nextNum Three = Four
nextNum Four = Five
nextNum Five = Six
nextNum Six = None

viewer :: DeviceContext -> TVar State -> IO ()
viewer context state_var = do
        state <- readTVarIO state_var
        let page = thePage state
        (w,h,sz) <- send context $ do
                let (w,h) = (width context, height context)
                clearRect (0,0,w,h)
                beginPath()

                let sz = min w h
                save()
                translate (w / 2, h / 2)
                sequence_ [ do bigLine (-sz * 0.5,n) (sz * 0.5,n)
                               bigLine (n,-sz * 0.5) (n,sz * 0.5)

                          | n <- [-sz * 0.5,sz * 0.5]
                          ]

                sequence_ [ do bigLine (-sz * 0.5,m) (sz * 0.5,m)
                               bigLine (m,-sz * 0.5) (m,sz * 0.5)
                               bigLine (0,-sz * 0.5) (0,sz * 0.5)
                               bigLine (-sz * 0.5,0) (sz * 0.5,0)

                          | m <- [-sz * 0.25,sz * 0.25]
                          ]

                sequence_ [ do smallLine (-sz * 0.5,m) (sz * 0.5,m)
                               smallLine (m,-sz * 0.5) (m,sz * 0.5)
                               smallLine (-sz * 0.5,m*3) (sz * 0.5,m*3)
                               smallLine (m*3,-sz * 0.5) (m*3,sz * 0.5)

                          | m <- [-sz * 0.125,sz * 0.125]
                          ]

                sequence_ [ do smallLine (-sz * 0.5,m) (sz * 0.5,m)
                               smallLine (m,-sz * 0.5) (m,sz * 0.5)
                               smallLine (-sz * 0.5,m*3) (sz * 0.5,m*3)
                               smallLine (m*3,-sz * 0.5) (m*3,sz * 0.5)
                               smallLine (-sz * 0.5,m*5) (sz * 0.5,m*5)
                               smallLine (m*5,-sz * 0.5) (m*5,sz * 0.5)
                               smallLine (-sz * 0.5,m*7) (sz * 0.5,m*7)
                               smallLine (7*m,-sz * 0.5) (7*m,sz * 0.5)

                          | m <- [-sz * 0.0625,sz * 0.0625]
                          ]

                sequence_ [ do drawX (0.5)
                          ]    


                --sequence_ [ do save()
                --               translate (w / 2, h / 2)
                --               case Map.lookup (x,y) page of
                --                  Just None -> drawX (sz * 0.05)
                --                  Just Zero -> drawX (sz * 0.05)
                --                  Just One -> drawX (sz * 0.05)
                --                  Just Two -> drawX (sz * 0.05)
                --                  Just Three -> drawX (sz * 0.05)
                --                  Just Four -> drawX (sz * 0.05)
                --                  Just Five -> drawX (sz * 0.05)
                --                  Just Six -> drawX (sz * 0.05)
                --                  Nothing -> return ()
                --               restore()
                --          | x <- [-8,-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7,8]
                --          , y <- [-8,-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7,8]
                --          ]
                restore()
                return (w,h,sz)

        atomically $ do
          state' <- readTVar state_var
          if state' == state then retry else return ()
        viewer context state_var


control_loop :: DeviceContext -> TVar State -> IO ()
control_loop context state_var = do
--        print page
        state <- readTVarIO state_var
        let num = ident state
        let page = thePage state
        let (w,h) = (width context, height context)
        let sz = min w h

        event <- wait context
--        print event
        case ePageXY event of
           -- if no mouse location, ignore, and redraw
           Nothing -> control_loop context state_var
           Just (x',y') -> control_loop context state_var


xColor, pageColor :: Text
xColor = "ff0000"
pageColor = "#000080"

drawX :: Double -> Canvas ()
drawX size = do
        strokeStyle xColor
        lineCap "butt"
        beginPath()
        moveTo(-size,-size)
        lineTo(size,size)
        lineWidth 1
        stroke()
        beginPath()
        moveTo(-size,size)
        lineTo(size,-size)
        lineWidth 1
        stroke()

bigLine :: (Double, Double) -> (Double, Double) -> Canvas ()
bigLine (x,y) (x',y') = do
        beginPath()
        moveTo(x,y)
        lineTo(x',y')
        lineWidth 3
        strokeStyle pageColor
        lineCap "square"
        stroke()

smallLine :: (Double, Double) -> (Double, Double) -> Canvas ()
smallLine (x,y) (x',y') = do
        beginPath()
        moveTo(x,y)
        lineTo(x',y')
        lineWidth 1
        strokeStyle pageColor
        lineCap "square"
        stroke()

--------------


data State = State 
        { thePage   :: Map (Int, Int) NUM
        , ident     :: NUM
        } deriving (Show, Eq)

startState :: State
startState = State Map.empty None

click :: (Int,Int) -> NUM -> State -> State
click pos num state
        = case Map.lookup pos (thePage state) of
                   Nothing -> state { thePage = Map.insert pos num (thePage state)
                                    , ident = nextNum (ident state)
                                    }
                   Just {} -> state
