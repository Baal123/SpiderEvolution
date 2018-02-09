module Main(main) where
import           Data.Maybe
import           Graphics.Gloss
import           Graphics.Gloss.Juicy
import           Simulation
import           SimulationState

mapWithIndex = go 0
               where
                go i f []     = []
                go i f (x:xs) = f i x:go (i+1) f xs

width = 600
height = 600
offset = 0

cellWidth = 42 :: Float
cellHeight = 42 :: Float
xOffset = 100 :: Int
window :: Display
window = InWindow "SpiderSim" (width, height) (xOffset, offset)

fps = 4

background :: Color
background = white

wallP = loadJuicyPNG "Sprites/Dungeon/Wall_42_42.png"
spiderDP = loadJuicyPNG "Sprites/LPC_Spiders/Spider_Down_1.png"
spiderLP = loadJuicyPNG "Sprites/LPC_Spiders/Spider_Left_1.png"
spiderRP = loadJuicyPNG "Sprites/LPC_Spiders/Spider_Right_1.png"
spiderTP = loadJuicyPNG "Sprites/LPC_Spiders/Spider_Top_1.png"

data Sprites = Sprites {wall        :: Picture,
                        spiderDown  :: Picture,
                        spiderUp    :: Picture,
                        spiderLeft  :: Picture,
                        spiderRight :: Picture}

loadSprites :: Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Sprites
loadSprites w sD sT sL sR = Sprites {wall = defaultP "Could not load wall" w,
                                     spiderDown = defaultP "Could not load spider" sD,
                                     spiderLeft = defaultP "Could not load spider" sL,
                                     spiderRight = defaultP "Could not load spider" sR,
                                     spiderUp = defaultP "Could not load spider" sT}
                            where defaultP msg = fromMaybe (text msg)

main :: IO ()
main = do wall <- wallP
          spiderD <- spiderDP
          spiderL <- spiderLP
          spiderR <- spiderRP
          spiderT <- spiderTP
          startSim (loadSprites wall spiderD spiderT spiderL spiderR)

startSim info
  = simulate window background fps initialState (render info) update


update :: a -> Float -> StateWithTime -> StateWithTime
update _ t state = let state' = addTime t state
                       in simulateWorld defaultSpider state'

render :: Sprites -> StateWithTime -> Picture
render info (StateWithTime _ (SimState (World field xSize ySize) spiderPos))
  = scale 0.5 0.5 $ pictures [drawField info field, drawSpider info spiderPos]

drawSpider :: Sprites -> Position -> Picture
drawSpider sprites (x, y, o)
  = let pic = case o of
              Top     -> spiderUp sprites
              Bot     -> spiderDown sprites
              ToLeft  -> spiderLeft sprites
              ToRight -> spiderRight sprites
    in translate (fromIntegral x * cellWidth) (fromIntegral y * cellHeight) pic

drawField :: Sprites -> [[Field]] -> Picture
drawField sprites field = pictures $ mapWithIndex (drawRow sprites) field
drawRow sprites y row = pictures $ mapWithIndex (draw sprites y) row


draw sprites y x Empty = translate (x*cellWidth - fromIntegral xOffset) (y*cellHeight) $ color white $ rectangleSolid cellWidth cellHeight
draw sprites y x Wall = translate (x*cellWidth - fromIntegral xOffset) (y*cellHeight) $ wall sprites
draw sprites y x Goal = translate (x*cellWidth - fromIntegral xOffset) (y*cellHeight) $ color yellow $ rectangleSolid cellWidth cellHeight
