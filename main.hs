{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StaticPointers #-}

import CodeWorld hiding ((&))
import Control.Lens
import Control.Monad
import Control.Monad.State

import qualified Data.Map as M
import Data.Monoid

import Data.Generics.Labels

import GHC.Generics

import System.Random

data Bait = Bait
  { pos :: Point
  } deriving Generic

data Player = Player
  { pos :: Point
  , color :: Color
  , size :: Double
  , speed :: Point
  , force :: Point
  } deriving Generic

data World= World
  { baits :: [Bait]
  , players :: M.Map Int Player
  , seed :: StdGen
  } deriving Generic

baseSpeed :: Double
baseSpeed = 5

applySpeed :: Point -> Double -> Point -> Point 
applySpeed (vx, vy) dt (x,y) = (x + dt * vx, y + dt * vy)

playerColors = cycle [purple, black, red, blue, orange]

mkPlayer :: Color -> Int -> Player 
mkPlayer c i = 
  let angle = (fromIntegral i) * 2 * pi/playerCount :: Double
      r = 5
  in spawn c (r*sin(angle),r*cos(angle)) 

spawn :: Color -> Point -> Player 
spawn c pos = Player pos c 1 (0,0) (0,0)

initWorld seed = World baits players seed
  where baits = do
          x <- [-5, 5]
          y <- [-5, 5]
          return $ Bait (x,y)
        players = M.fromList $ 
          zip playerColors playerIdxs <&> \(c,i) -> (i, mkPlayer c i)

gameField :: (Point, Point)
gameField = ((-10,-10), (10,10))

genBaits :: Double -> World -> World
genBaits dt w = if length (w ^. #baits) > 10
  then w
  else newWorld
  where newWorld =
          let (l, newSeed) = randomR (0,1) (w ^. #seed)
              (bx, newSeed2) = randomR (-10,10) newSeed
              (by, newSeed3) = randomR (-10,10) newSeed2
          in w 
            { seed = newSeed3
            , baits = w ^. #baits ++ 
                if (l < dt) then [Bait (bx,by)] else []
            }

newtype PlayerRef = PlayerRef (Lens' World Player)

randomDouble :: (Double, Double) -> State World Double
randomDouble range = do 
  oldSeed <- use #seed
  let (res, newSeed) = randomR range oldSeed
  #seed .= newSeed
  return res

randomPos :: (Point, Point) -> State World Point
randomPos ((p1x, p1y), (p2x, p2y)) = (,) 
  <$> randomDouble (p1x, p2x) 
  <*> randomDouble (p1y, p2y) 

handlePvP :: World -> World 
handlePvP w = flip execState w $ do
  let mkRef i = PlayerRef $ #players . unsafeSingular (ix i)
      allPairs = [(mkRef iLeft, mkRef iRight)
        | iLeft <- playerIdxs
        , iRight <- playerIdxs
        , iLeft /= iRight 
        ]
  forM_ allPairs $ \(PlayerRef refEater, PlayerRef refBait) -> do
    pEater <- use refEater
    pBait <- use refBait
    let (bx, by) = pBait ^. #pos
        (px, py) = pEater ^. #pos
        dist = sqrt $ ((px-bx)^2 + (py-by)^2) 
        isEaten = (pBait ^. #size < (pEater ^. #size) * 0.9) && (dist < pEater ^. #size)
        
    when isEaten $ do
      refEater . #size += pBait ^. #size
      newPos <- randomPos gameField
      refBait .= spawn (pBait ^. #color) newPos

handleBaits :: PlayerRef -> World -> World
handleBaits (PlayerRef playerRef) world = newWorld
  where newWorld = world 
          & #baits .~ newBaits
          & playerRef . #size %~ \size ->
            sqrt (size^2 + eatenCount * 0.1)
        (Sum eatenCount, newBaits) = 
          flip filterM (world ^. #baits) $ \b -> if isEaten b 
            then (Sum 1, False)
            else (Sum 0, True)
        isEaten :: Bait -> Bool 
        isEaten b = dist < (p ^. #size)
          where p = world ^. playerRef
                (bx, by) = (b ^. #pos)
                (px, py) = p ^. #pos
                dist = sqrt $ ((px-bx)^2 + (py-by)^2) 

handleEvent :: Int -> Event -> World -> World
handleEvent playerNum e world = case e of
  KeyPress "W" -> changeForce $ _2 .~ 1 --  adjSpeed
  KeyPress "S" -> changeForce $ _2 .~ (-1) -- (-adjSpeed)
  KeyPress "A" -> changeForce $ _1 .~ (-1) -- (-adjSpeed)
  KeyPress "D" -> changeForce $ _1 .~ 1 -- adjSpeed
  KeyRelease "W" -> changeForce $ _2 .~ 0
  KeyRelease "S" -> changeForce $ _2 .~ 0
  KeyRelease "A" -> changeForce $ _1 .~ 0
  KeyRelease "D" -> changeForce $ _1 .~ 0
  TimePassing dt -> handlePvP $ genBaits dt $ handleBaits' $ 
    world & #players . traverse %~ \p -> p
      & #pos %~ applySpeed (p ^. #speed) dt
      & #speed %~
         (both %~ attenuate (p ^. #size) dt) .
           (applySpeed (p ^. #force) dt)
  _ -> world
  where changeForce x = world & #players . ix playerNum . #force %~ x
        attenuate size dt s = s * exp(-dt * size)
        handleBaits' world = foldr (\ref w -> handleBaits ref w) world $ 
          [0..playerCount-1] <&> \i -> PlayerRef (#players . unsafeSingular(ix i))

renderPlayer :: Player -> Picture
renderPlayer p = translated x y $ 
  colored (p ^. #color) $ circle $ size p
  where (x,y) = p ^. #pos

renderBait :: Bait -> Picture
renderBait b = translated x y $ solidRectangle 0.3 0.3
  where (x,y) = b ^. #pos

renderWorld :: World -> Picture
renderWorld world = playersPic <> baits
  where playersPic = foldMap renderPlayer $  world ^. #players
        baits = mconcat $ 
          renderBait <$> (world ^. #baits)

playerIdxs :: [Int]
playerIdxs = [0..playerCount-1] 

playerCount :: Num a => a
playerCount = 1

{--
main = do
  seed <- getStdGen
  activityOf (initWorld seed) handleEvent renderWorld
--}

{--}
main = groupActivityOf playerCount init step view
  where
    init = static (initWorld)
    step = static handleEvent
    view = static (\playerNumber -> renderWorld)
--}
