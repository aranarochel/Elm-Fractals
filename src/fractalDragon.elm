import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import List exposing (..)
import Random
import Window
import Keyboard
 
type alias Point = (Float, Float)
 
type alias Model =
  { points : List Point
  , level : Int
  , frame : Int
  , seed : Random.Seed
  , offsetM : Float
  }
 
-- maxLevel controls how detailed we want the image
buildLevel = 12
endLevel = 24

-- frameCount controls the speed of the animation
-- higher numbers means it goes slower
frameCount = 450
seed0 = Random.initialSeed 31415

-- probability is a random number between the ones specified below (0.0,1.75)
probability : Random.Generator Float
probability =
    Random.float 1.0 1.0

-- this is the starting model
-- you can change the start points below or maybe we can do random start points
-- maybe we can even add more initial points to the model at start                               
init : Model
init = { points = [(-370.0, -110.0), (370.0, -110.0)]
       , level = 0
       , frame = 0
       , seed = seed0
       , offsetM = 1.0
       }
 
-- New point between two existing points.  Offset to left or right
newPoint : Point -> Point -> Float -> Point
newPoint  (x0,y0) (x1,y1) offset =
  let (vx, vy) = ((x1 - x0) / 2.0, (y1 - y0) / 2.0)
      (dx, dy) = (-vy * offset , vx * offset )
  in  (x0 + vx + dx, y0 + vy + dy) --offset from midpoint

-- New point between two existing points.  Offset to left or right
newPoint2 : Point -> Point -> Float -> Float -> Point
newPoint2  (x0,y0) (x1,y1) offset offsetM =
  let (vx, vy) = ((x1 - x0) / 2.0, (y1 - y0) / 2.0)
      (dx, dy) = (-vy * (offsetM-offset), vx * (offsetM-offset))
  in  (x0 + vx + dx, y0 + vy + dy) --offset from midpoint
 
-- Insert between existing points. Offset to left or right side.
newPoints : Int -> Float -> Float -> List Point -> List Point
newPoints lv offset offsetM points =
  if lv < buildLevel then
    case points of
      [] -> []
      [p0] -> [p0] 
      p0::p1::rest -> p0 :: newPoint p0 p1 offset :: newPoints lv -offset offsetM (p1::rest)
  else
    case points of
      [] -> []
      [p0] -> [p0] 
      p0::p1::[] -> [p1]
      p0::p1::p2::rest -> p0 :: newPoint2 p0 p2 offset offsetM :: newPoints lv (2*offsetM-offset) offsetM (p2::rest)
   
-- Remove points
rmPoints : List Point -> List Point
rmPoints points =
   case points of
   [] -> []
   [p0] -> [p0]
   p0::p1::rest -> p0::(rmPoints rest)

-- here we have two options
-- 1) The fractal continuously changes offset values during its maxLevel iterations
-- 2) The fractal sticks with one offset value, and only switches when it restarts animation
update : Model -> Model
update model = 
  let nextFrame = model.frame + 1
      (offset, seed') = Random.generate probability model.seed
  in if (model.level == endLevel) then {init | seed = seed', offsetM = offset}
     else if (model.level < buildLevel && nextFrame == frameCount) then
       { points = newPoints model.level model.offsetM model.offsetM model.points 
       , level = model.level+1
       , frame = 0
       , seed = seed'
       , offsetM = model.offsetM
       }
  else if (model.level < endLevel && nextFrame == frameCount) then
       { points = rmPoints model.points
       , level = model.level+1
       , frame = 0
       , seed = seed'
       , offsetM = model.offsetM
       }
     else
       { model | frame = nextFrame
       }
 
-- break a list up into n equal sized lists.
breakupInto : Int -> List a -> List (List a)
breakupInto n ls =
    let segmentCount = (List.length ls) - 1
        breakup n ls = case ls of
          [] -> []
          _ -> List.take (n+1) ls :: breakup n (List.drop n ls)
    in if n > segmentCount
       then [ls]
       else breakup (segmentCount // n) ls
 
view (w,h) model =
  let offset = model.offsetM * toFloat (model.frame) / toFloat frameCount
      -- you can add or delete colors here. There should be a full list on the website
      colors = [red, orange, green, blue]
  in layers
       [ collage w h -- these numbers controls how big the box around the image is
         (model.points
           |> newPoints model.level offset model.offsetM
           |> breakupInto (List.length colors) -- for coloring
           |> List.map path
           |> List.map2 (\color path -> traced (solid color) path ) colors )
         , show "Dragon"
       ]
main =
  Signal.foldp (\_ c -> c+1) 0 (every (5*millisecond))
     |> Signal.foldp (\_ d -> update d) init 
     |> Signal.map2 view Window.dimensions