{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import Control.Concurrent.STM
import Control.Concurrent
import Data.Text

import           Graphics.Blank
import           Game

main :: IO ()
main = do
  blankCanvas 3000 { events = ["mousedown"] } $ \ context -> do
        forkIO $ viewer context
        control_loop context puzzle1
        return ()

viewer :: DeviceContext -> IO ()
viewer context = do
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

                let x = sz*0.0615
                let y = sz*0.0630
                let z = sz*0.0640
                let w = sz*0.0615
                let n = sz*0.01
                let m = -sz*0.006

                let fsz = (sz/16)-2
                let fszs = pack(show fsz ++ "pt Courier")

                sequence_ [ do font fszs

                               fillText(singleton(charList !! 0 !! 0), -x*8, -y*7)
                               fillText(singleton(charList !! 0 !! 1), -x*7, -y*7)
                               fillText(singleton(charList !! 0 !! 2), -x*6, -y*7)
                               fillText(singleton(charList !! 0 !! 3), -x*5, -y*7)
                               fillText(singleton(charList !! 0 !! 4), -x*4, -y*7)
                               fillText(singleton(charList !! 0 !! 5), -x*3, -y*7)
                               fillText(singleton(charList !! 0 !! 6), -x*2, -y*7)
                               fillText(singleton(charList !! 0 !! 7), -x*1, -y*7)
                               fillText(singleton(charList !! 0 !! 8), n, -y*7)
                               fillText(singleton(charList !! 0 !! 9), z*1, -y*7)
                               fillText(singleton(charList !! 0 !! 10), z*2, -y*7)
                               fillText(singleton(charList !! 0 !! 11), z*3, -y*7)
                               fillText(singleton(charList !! 0 !! 12), z*4, -y*7)
                               fillText(singleton(charList !! 0 !! 13), z*5, -y*7)
                               fillText(singleton(charList !! 0 !! 14), z*6, -y*7)
                               fillText(singleton(charList !! 0 !! 15), z*7, -y*7)

                               fillText(singleton(charList !! 1 !! 0), -x*8, -y*6)
                               fillText(singleton(charList !! 1 !! 1), -x*7, -y*6)
                               fillText(singleton(charList !! 1 !! 2), -x*6, -y*6)
                               fillText(singleton(charList !! 1 !! 3), -x*5, -y*6)
                               fillText(singleton(charList !! 1 !! 4), -x*4, -y*6)
                               fillText(singleton(charList !! 1 !! 5), -x*3, -y*6)
                               fillText(singleton(charList !! 1 !! 6), -x*2, -y*6)
                               fillText(singleton(charList !! 1 !! 7), -x*1, -y*6)
                               fillText(singleton(charList !! 1 !! 8), n, -y*6)
                               fillText(singleton(charList !! 1 !! 9), z*1, -y*6)
                               fillText(singleton(charList !! 1 !! 10), z*2, -y*6)
                               fillText(singleton(charList !! 1 !! 11), z*3, -y*6)
                               fillText(singleton(charList !! 1 !! 12), z*4, -y*6)
                               fillText(singleton(charList !! 1 !! 13), z*5, -y*6)
                               fillText(singleton(charList !! 1 !! 14), z*6, -y*6)
                               fillText(singleton(charList !! 1 !! 15), z*7, -y*6)

                               fillText(singleton(charList !! 2 !! 0), -x*8, -y*5)
                               fillText(singleton(charList !! 2 !! 1), -x*7, -y*5)
                               fillText(singleton(charList !! 2 !! 2), -x*6, -y*5)
                               fillText(singleton(charList !! 2 !! 3), -x*5, -y*5)
                               fillText(singleton(charList !! 2 !! 4), -x*4, -y*5)
                               fillText(singleton(charList !! 2 !! 5), -x*3, -y*5)
                               fillText(singleton(charList !! 2 !! 6), -x*2, -y*5)
                               fillText(singleton(charList !! 2 !! 7), -x*1, -y*5)
                               fillText(singleton(charList !! 2 !! 8), n, -y*5)
                               fillText(singleton(charList !! 2 !! 9), z*1, -y*5)
                               fillText(singleton(charList !! 2 !! 10), z*2, -y*5)
                               fillText(singleton(charList !! 2 !! 11), z*3, -y*5)
                               fillText(singleton(charList !! 2 !! 12), z*4, -y*5)
                               fillText(singleton(charList !! 2 !! 13), z*5, -y*5)
                               fillText(singleton(charList !! 2 !! 14), z*6, -y*5)
                               fillText(singleton(charList !! 2 !! 15), z*7, -y*5)

                               fillText(singleton(charList !! 3 !! 0), -x*8, -y*4)
                               fillText(singleton(charList !! 3 !! 1), -x*7, -y*4)
                               fillText(singleton(charList !! 3 !! 2), -x*6, -y*4)
                               fillText(singleton(charList !! 3 !! 3), -x*5, -y*4)
                               fillText(singleton(charList !! 3 !! 4), -x*4, -y*4)
                               fillText(singleton(charList !! 3 !! 5), -x*3, -y*4)
                               fillText(singleton(charList !! 3 !! 6), -x*2, -y*4)
                               fillText(singleton(charList !! 3 !! 7), -x*1, -y*4)
                               fillText(singleton(charList !! 3 !! 8), n, -y*4)
                               fillText(singleton(charList !! 3 !! 9), z*1, -y*4)
                               fillText(singleton(charList !! 3 !! 10), z*2, -y*4)
                               fillText(singleton(charList !! 3 !! 11), z*3, -y*4)
                               fillText(singleton(charList !! 3 !! 12), z*4, -y*4)
                               fillText(singleton(charList !! 3 !! 13), z*5, -y*4)
                               fillText(singleton(charList !! 3 !! 14), z*6, -y*4)
                               fillText(singleton(charList !! 3 !! 15), z*7, -y*4)

                               fillText(singleton(charList !! 4 !! 0), -x*8, -y*3)
                               fillText(singleton(charList !! 4 !! 1), -x*7, -y*3)
                               fillText(singleton(charList !! 4 !! 2), -x*6, -y*3)
                               fillText(singleton(charList !! 4 !! 3), -x*5, -y*3)
                               fillText(singleton(charList !! 4 !! 4), -x*4, -y*3)
                               fillText(singleton(charList !! 4 !! 5), -x*3, -y*3)
                               fillText(singleton(charList !! 4 !! 6), -x*2, -y*3)
                               fillText(singleton(charList !! 4 !! 7), -x*1, -y*3)
                               fillText(singleton(charList !! 4 !! 8), n, -y*3)
                               fillText(singleton(charList !! 4 !! 9), z*1, -y*3)
                               fillText(singleton(charList !! 4 !! 10), z*2, -y*3)
                               fillText(singleton(charList !! 4 !! 11), z*3, -y*3)
                               fillText(singleton(charList !! 4 !! 12), z*4, -y*3)
                               fillText(singleton(charList !! 4 !! 13), z*5, -y*3)
                               fillText(singleton(charList !! 4 !! 14), z*6, -y*3)
                               fillText(singleton(charList !! 4 !! 15), z*7, -y*3)

                               fillText(singleton(charList !! 5 !! 0), -x*8, -y*2)
                               fillText(singleton(charList !! 5 !! 1), -x*7, -y*2)
                               fillText(singleton(charList !! 5 !! 2), -x*6, -y*2)
                               fillText(singleton(charList !! 5 !! 3), -x*5, -y*2)
                               fillText(singleton(charList !! 5 !! 4), -x*4, -y*2)
                               fillText(singleton(charList !! 5 !! 5), -x*3, -y*2)
                               fillText(singleton(charList !! 5 !! 6), -x*2, -y*2)
                               fillText(singleton(charList !! 5 !! 7), -x*1, -y*2)
                               fillText(singleton(charList !! 5 !! 8), n, -y*2)
                               fillText(singleton(charList !! 5 !! 9), z*1, -y*2)
                               fillText(singleton(charList !! 5 !! 10), z*2, -y*2)
                               fillText(singleton(charList !! 5 !! 11), z*3, -y*2)
                               fillText(singleton(charList !! 5 !! 12), z*4, -y*2)
                               fillText(singleton(charList !! 5 !! 13), z*5, -y*2)
                               fillText(singleton(charList !! 5 !! 14), z*6, -y*2)
                               fillText(singleton(charList !! 5 !! 15), z*7, -y*2)

                               fillText(singleton(charList !! 6 !! 0), -x*8, -y*1)
                               fillText(singleton(charList !! 6 !! 1), -x*7, -y*1)
                               fillText(singleton(charList !! 6 !! 2), -x*6, -y*1)
                               fillText(singleton(charList !! 6 !! 3), -x*5, -y*1)
                               fillText(singleton(charList !! 6 !! 4), -x*4, -y*1)
                               fillText(singleton(charList !! 6 !! 5), -x*3, -y*1)
                               fillText(singleton(charList !! 6 !! 6), -x*2, -y*1)
                               fillText(singleton(charList !! 6 !! 7), -x*1, -y*1)
                               fillText(singleton(charList !! 6 !! 8), n, -y*1)
                               fillText(singleton(charList !! 6 !! 9), z*1, -y*1)
                               fillText(singleton(charList !! 6 !! 10), z*2, -y*1)
                               fillText(singleton(charList !! 6 !! 11), z*3, -y*1)
                               fillText(singleton(charList !! 6 !! 12), z*4, -y*1)
                               fillText(singleton(charList !! 6 !! 13), z*5, -y*1)
                               fillText(singleton(charList !! 6 !! 14), z*6, -y*1)
                               fillText(singleton(charList !! 6 !! 15), z*7, -y*1)

                               fillText(singleton(charList !! 7 !! 0), -x*8, m)
                               fillText(singleton(charList !! 7 !! 1), -x*7, m)
                               fillText(singleton(charList !! 7 !! 2), -x*6, m)
                               fillText(singleton(charList !! 7 !! 3), -x*5, m)
                               fillText(singleton(charList !! 7 !! 4), -x*4, m)
                               fillText(singleton(charList !! 7 !! 5), -x*3, m)
                               fillText(singleton(charList !! 7 !! 6), -x*2, m)
                               fillText(singleton(charList !! 7 !! 7), -x*1, m)
                               fillText(singleton(charList !! 7 !! 8), n, m)
                               fillText(singleton(charList !! 7 !! 9), z*1, m)
                               fillText(singleton(charList !! 7 !! 10), z*2, m)
                               fillText(singleton(charList !! 7 !! 11), z*3, m)
                               fillText(singleton(charList !! 7 !! 12), z*4, m)
                               fillText(singleton(charList !! 7 !! 13), z*5, m)
                               fillText(singleton(charList !! 7 !! 14), z*6, m)
                               fillText(singleton(charList !! 7 !! 15), z*7, m)

                               fillText(singleton(charList !! 8 !! 0), -x*8, w*1)
                               fillText(singleton(charList !! 8 !! 1), -x*7, w*1)
                               fillText(singleton(charList !! 8 !! 2), -x*6, w*1)
                               fillText(singleton(charList !! 8 !! 3), -x*5, w*1)
                               fillText(singleton(charList !! 8 !! 4), -x*4, w*1)
                               fillText(singleton(charList !! 8 !! 5), -x*3, w*1)
                               fillText(singleton(charList !! 8 !! 6), -x*2, w*1)
                               fillText(singleton(charList !! 8 !! 7), -x*1, w*1)
                               fillText(singleton(charList !! 8 !! 8), n, w*1)
                               fillText(singleton(charList !! 8 !! 9), z*1, w*1)
                               fillText(singleton(charList !! 8 !! 10), z*2, w*1)
                               fillText(singleton(charList !! 8 !! 11), z*3, w*1)
                               fillText(singleton(charList !! 8 !! 12), z*4, w*1)
                               fillText(singleton(charList !! 8 !! 13), z*5, w*1)
                               fillText(singleton(charList !! 8 !! 14), z*6, w*1)
                               fillText(singleton(charList !! 8 !! 15), z*7, w*1)

                               fillText(singleton(charList !! 9 !! 0), -x*8, w*2)
                               fillText(singleton(charList !! 9 !! 1), -x*7, w*2)
                               fillText(singleton(charList !! 9 !! 2), -x*6, w*2)
                               fillText(singleton(charList !! 9 !! 3), -x*5, w*2)
                               fillText(singleton(charList !! 9 !! 4), -x*4, w*2)
                               fillText(singleton(charList !! 9 !! 5), -x*3, w*2)
                               fillText(singleton(charList !! 9 !! 6), -x*2, w*2)
                               fillText(singleton(charList !! 9 !! 7), -x*1, w*2)
                               fillText(singleton(charList !! 9 !! 8), n, w*2)
                               fillText(singleton(charList !! 9 !! 9), z*1, w*2)
                               fillText(singleton(charList !! 9 !! 10), z*2, w*2)
                               fillText(singleton(charList !! 9 !! 11), z*3, w*2)
                               fillText(singleton(charList !! 9 !! 12), z*4, w*2)
                               fillText(singleton(charList !! 9 !! 13), z*5, w*2)
                               fillText(singleton(charList !! 9 !! 14), z*6, w*2)
                               fillText(singleton(charList !! 9 !! 15), z*7, w*2)

                               fillText(singleton(charList !! 10 !! 0), -x*8, w*3)
                               fillText(singleton(charList !! 10 !! 1), -x*7, w*3)
                               fillText(singleton(charList !! 10 !! 2), -x*6, w*3)
                               fillText(singleton(charList !! 10 !! 3), -x*5, w*3)
                               fillText(singleton(charList !! 10 !! 4), -x*4, w*3)
                               fillText(singleton(charList !! 10 !! 5), -x*3, w*3)
                               fillText(singleton(charList !! 10 !! 6), -x*2, w*3)
                               fillText(singleton(charList !! 10 !! 7), -x*1, w*3)
                               fillText(singleton(charList !! 10 !! 8), n, w*3)
                               fillText(singleton(charList !! 10 !! 9), z*1, w*3)
                               fillText(singleton(charList !! 10 !! 10), z*2, w*3)
                               fillText(singleton(charList !! 10 !! 11), z*3, w*3)
                               fillText(singleton(charList !! 10 !! 12), z*4, w*3)
                               fillText(singleton(charList !! 10 !! 13), z*5, w*3)
                               fillText(singleton(charList !! 10 !! 14), z*6, w*3)
                               fillText(singleton(charList !! 10 !! 15), z*7, w*3)

                               fillText(singleton(charList !! 11 !! 0), -x*8, w*4)
                               fillText(singleton(charList !! 11 !! 1), -x*7, w*4)
                               fillText(singleton(charList !! 11 !! 2), -x*6, w*4)
                               fillText(singleton(charList !! 11 !! 3), -x*5, w*4)
                               fillText(singleton(charList !! 11 !! 4), -x*4, w*4)
                               fillText(singleton(charList !! 11 !! 5), -x*3, w*4)
                               fillText(singleton(charList !! 11 !! 6), -x*2, w*4)
                               fillText(singleton(charList !! 11 !! 7), -x*1, w*4)
                               fillText(singleton(charList !! 11 !! 8), n, w*4)
                               fillText(singleton(charList !! 11 !! 9), z*1, w*4)
                               fillText(singleton(charList !! 11 !! 10), z*2, w*4)
                               fillText(singleton(charList !! 11 !! 11), z*3, w*4)
                               fillText(singleton(charList !! 11 !! 12), z*4, w*4)
                               fillText(singleton(charList !! 11 !! 13), z*5, w*4)
                               fillText(singleton(charList !! 11 !! 14), z*6, w*4)
                               fillText(singleton(charList !! 11 !! 15), z*7, w*4)

                               fillText(singleton(charList !! 12 !! 0), -x*8, w*5)
                               fillText(singleton(charList !! 12 !! 1), -x*7, w*5)
                               fillText(singleton(charList !! 12 !! 2), -x*6, w*5)
                               fillText(singleton(charList !! 12 !! 3), -x*5, w*5)
                               fillText(singleton(charList !! 12 !! 4), -x*4, w*5)
                               fillText(singleton(charList !! 12 !! 5), -x*3, w*5)
                               fillText(singleton(charList !! 12 !! 6), -x*2, w*5)
                               fillText(singleton(charList !! 12 !! 7), -x*1, w*5)
                               fillText(singleton(charList !! 12 !! 8), n, w*5)
                               fillText(singleton(charList !! 12 !! 9), z*1, w*5)
                               fillText(singleton(charList !! 12 !! 10), z*2, w*5)
                               fillText(singleton(charList !! 12 !! 11), z*3, w*5)
                               fillText(singleton(charList !! 12 !! 12), z*4, w*5)
                               fillText(singleton(charList !! 12 !! 13), z*5, w*5)
                               fillText(singleton(charList !! 12 !! 14), z*6, w*5)
                               fillText(singleton(charList !! 12 !! 15), z*7, w*5)

                               fillText(singleton(charList !! 13 !! 0), -x*8, w*6)
                               fillText(singleton(charList !! 13 !! 1), -x*7, w*6)
                               fillText(singleton(charList !! 13 !! 2), -x*6, w*6)
                               fillText(singleton(charList !! 13 !! 3), -x*5, w*6)
                               fillText(singleton(charList !! 13 !! 4), -x*4, w*6)
                               fillText(singleton(charList !! 13 !! 5), -x*3, w*6)
                               fillText(singleton(charList !! 13 !! 6), -x*2, w*6)
                               fillText(singleton(charList !! 13 !! 7), -x*1, w*6)
                               fillText(singleton(charList !! 13 !! 8), n, w*6)
                               fillText(singleton(charList !! 13 !! 9), z*1, w*6)
                               fillText(singleton(charList !! 13 !! 10), z*2, w*6)
                               fillText(singleton(charList !! 13 !! 11), z*3, w*6)
                               fillText(singleton(charList !! 13 !! 12), z*4, w*6)
                               fillText(singleton(charList !! 13 !! 13), z*5, w*6)
                               fillText(singleton(charList !! 13 !! 14), z*6, w*6)
                               fillText(singleton(charList !! 13 !! 15), z*7, w*6)

                               fillText(singleton(charList !! 14 !! 0), -x*8, w*7)
                               fillText(singleton(charList !! 14 !! 1), -x*7, w*7)
                               fillText(singleton(charList !! 14 !! 2), -x*6, w*7)
                               fillText(singleton(charList !! 14 !! 3), -x*5, w*7)
                               fillText(singleton(charList !! 14 !! 4), -x*4, w*7)
                               fillText(singleton(charList !! 14 !! 5), -x*3, w*7)
                               fillText(singleton(charList !! 14 !! 6), -x*2, w*7)
                               fillText(singleton(charList !! 14 !! 7), -x*1, w*7)
                               fillText(singleton(charList !! 14 !! 8), n, w*7)
                               fillText(singleton(charList !! 14 !! 9), z*1, w*7)
                               fillText(singleton(charList !! 14 !! 10), z*2, w*7)
                               fillText(singleton(charList !! 14 !! 11), z*3, w*7)
                               fillText(singleton(charList !! 14 !! 12), z*4, w*7)
                               fillText(singleton(charList !! 14 !! 13), z*5, w*7)
                               fillText(singleton(charList !! 14 !! 14), z*6, w*7)
                               fillText(singleton(charList !! 14 !! 15), z*7, w*7)

                               fillText(singleton(charList !! 15 !! 0), -x*8, w*8)
                               fillText(singleton(charList !! 15 !! 1), -x*7, w*8)
                               fillText(singleton(charList !! 15 !! 2), -x*6, w*8)
                               fillText(singleton(charList !! 15 !! 3), -x*5, w*8)
                               fillText(singleton(charList !! 15 !! 4), -x*4, w*8)
                               fillText(singleton(charList !! 15 !! 5), -x*3, w*8)
                               fillText(singleton(charList !! 15 !! 6), -x*2, w*8)
                               fillText(singleton(charList !! 15 !! 7), -x*1, w*8)
                               fillText(singleton(charList !! 15 !! 8), n, w*8)
                               fillText(singleton(charList !! 15 !! 9), z*1, w*8)
                               fillText(singleton(charList !! 15 !! 10), z*2, w*8)
                               fillText(singleton(charList !! 15 !! 11), z*3, w*8)
                               fillText(singleton(charList !! 15 !! 12), z*4, w*8)
                               fillText(singleton(charList !! 15 !! 13), z*5, w*8)
                               fillText(singleton(charList !! 15 !! 14), z*6, w*8)
                               fillText(singleton(charList !! 15 !! 15), z*7, w*8)

                          ]

                restore()
                return (w,h,sz)

        viewer context


control_loop :: DeviceContext -> [[Char]] -> IO [[Char]]
control_loop context puzzle1 = do
--        print page
        let (w,h) = (width context, height context)
        let sz = min w h

        let charList = start puzzle1 -- :: [[[Char]]]

        -- do stuff

        control_loop context charList

--        event <- wait context
----        print event
--        case ePageXY event of
--           -- if no mouse location, ignore, and redraw
--           Nothing -> control_loop context
--           Just (x',y') -> control_loop context


pageColor :: Text
pageColor = "#000080"

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


--charList :: [[Char]]
--charList = [
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f'],
--           ['0', '1', '2', '3', '4', '5', '6', '7','8','9','a','b','c','d','e','f']
--         ]