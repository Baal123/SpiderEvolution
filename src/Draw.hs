module Draw where
import SimulationState
import Graphics.Gloss
import Codec.BMP


draw cellWidth cellHeight y x Empty = translate (x*cellWidth) (y*cellHeight) $ color white $ rectangleSolid cellWidth cellHeight
draw cellWidth cellHeight y x Wall = translate (x*cellWidth) (y*cellHeight) $ color black $ rectangleSolid cellWidth cellHeight
draw cellWidth cellHeight y x Goal = translate (x*cellWidth) (y*cellHeight) $ color yellow $ rectangleSolid cellWidth cellHeight
