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

data XO = X | O
        deriving (Eq,Ord,Show)

swap :: XO -> XO
swap X = O
swap O = X

viewer :: DeviceContext -> TVar State -> IO ()
viewer context state_var = do
        state <- readTVarIO state_var
        let board = theBoard state
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


                sequence_ [ do save()
                               translate (fromIntegral x * sz * 0.3,fromIntegral y * sz * 0.3)
                               case Map.lookup (x,y) board of
                                  Just X -> drawX (sz * 0.1)
                                  Just O -> drawO (sz * 0.1)
                                  Nothing -> return ()
                               restore()
                          | x <- [-1,0,1]
                          , y <- [-1,0,1]
                          ]
                restore()
                return (w,h,sz)

        atomically $ do
          state' <- readTVar state_var
          if state' == state then retry else return ()
        viewer context state_var


control_loop :: DeviceContext -> TVar State -> IO ()
control_loop context state_var = do
--        print board
--        print turn
        state <- readTVarIO state_var
        let turn = who state
        let board = theBoard state
        let (w,h) = (width context, height context)
        let sz = min w h
        let pointToSq :: (Double, Double) -> Maybe (Int,Int)
            pointToSq (x,y) = do
                    x' <- fd ((x - w / 2) / sz)
                    y' <- fd ((y - h / 2) / sz)
                    return (x',y')

            fd x = 
                    if r `elem` [-1..1] then Just (signum r) else Nothing
                where r = round (x * 3.3333)

        event <- wait context
--        print event
        case ePageXY event of
           -- if no mouse location, ignore, and redraw
           Nothing -> control_loop context state_var
           Just (x',y') -> case pointToSq (x',y') of
                             Nothing -> control_loop context state_var
                             Just pos -> do
                                atomically $ do
                                        state' <- readTVar state_var
                                        let turn = who state'
                                        writeTVar state_var (click pos turn state')         
                                control_loop context state_var


xColor, oColor, boardColor :: Text
xColor = "#ff0000"
oColor = "#00a000"
boardColor = "#000080"

--draw1 :: Double -> Canvas()
--        context.font = ''

drawX :: Double -> Canvas ()
drawX size = do
        strokeStyle xColor
        lineCap "butt"
        beginPath()
        moveTo(-size,-size)
        lineTo(size,size)
        lineWidth 10
        stroke()
        beginPath()
        moveTo(-size,size)
        lineTo(size,-size)
        lineWidth 10
        stroke()

drawO :: Double -> Canvas ()
drawO radius = do
        beginPath()
        arc(0, 0, radius, 0, 2 * pi, False)
        lineWidth 10
        strokeStyle oColor
        stroke()

bigLine :: (Double, Double) -> (Double, Double) -> Canvas ()
bigLine (x,y) (x',y') = do
        beginPath()
        moveTo(x,y)
        lineTo(x',y')
        lineWidth 3
        strokeStyle boardColor
        lineCap "square"
        stroke()

smallLine :: (Double, Double) -> (Double, Double) -> Canvas ()
smallLine (x,y) (x',y') = do
        beginPath()
        moveTo(x,y)
        lineTo(x',y')
        lineWidth 1
        strokeStyle boardColor
        lineCap "square"
        stroke()

--------------

data State = State 
        { theBoard   :: Map (Int, Int) XO
        , who        :: XO
        } deriving (Show, Eq)

startState :: State
startState = State Map.empty X

click :: (Int,Int) -> XO -> State -> State
click pos xo state
        = case Map.lookup pos (theBoard state) of
                   Nothing -> state { theBoard = Map.insert pos xo (theBoard state)
                                    , who = swap (who state) 
                                    }
                   Just {} -> state
