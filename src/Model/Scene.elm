module Model.Scene exposing (..)

import Keyboard exposing (KeyCode)
import Char exposing (toCode)
import Time exposing (Time)

import Model.Geometry exposing (..)


type alias Scene =
  { t : Time
  , player1 : Player
  , player2 : Player
  , round : Round }

type alias Player =
  { score : Int
  , homePosX : Float
  , leftKey : KeyCode
  , rightKey : KeyCode
  , jumpKey : KeyCode
  , position : Vector
  , velocity : Vector }

type alias Round =
  { touchdownTime : Time }

initialScene : Scene
initialScene =
  { t = 0
  , player1 = createPlayer 'A' 'D' 'F' 0.25
  , player2 = createPlayer 'H' 'K' 'L' 0.75
  , round = newRound }


createPlayer : Char -> Char -> Char ->  Float -> Player
createPlayer leftKey rightKey jumpKey posX =
  { score = 0
  , homePosX = posX
  , leftKey = Char.toCode leftKey
  , rightKey = Char.toCode rightKey
  , jumpKey = Char.toCode jumpKey
  , position = { x = posX, y = playerHomePosY }
  , velocity = { x = 0, y = 0 } }


newRound : Round
newRound =
  { touchdownTime = 0 }


playerHomePosY : Float
playerHomePosY =
  floorPosY-0.2

--TODO: floorPosY should be calculated based on min of height or width
floorPosY : Float
floorPosY = 0.89


floorPosX : Float
floorPosX = 0.1


floorRightEdgeX : Float
floorRightEdgeX = floorPosX + floorWidth


floorWidth : Float
floorWidth = 0.8


ceilingPosY : Float
ceilingPosY = 0.0


ceilingPosX : Float
ceilingPosX = 0.1


ceilingRightEdgeX : Float
ceilingRightEdgeX = ceilingPosX + ceilingWidth


ceilingWidth : Float
ceilingWidth = 0.8


playerRadius : Float
playerRadius = 0.03


players : Scene -> List Player
players scene =
  [ scene.player1, scene.player2 ]


playersOverlap : Player -> Player -> Bool
playersOverlap p1 p2 =
  let
      d = distance (p1.position.x,p1.position.y) (p2.position.x,p2.position.y)
  in
      d < playerRadius*2


playerHitCeiling : Player -> Bool
playerHitCeiling p =
  let
  --TODO: ceilingPosY+0.29 should be calculated
      d = distance (p.position.x,p.position.y) (p.position.x,ceilingPosY+0.29)
  in
      d < playerRadius*2


deflect : Player -> Player -> Vector
deflect player otherPlayer =
  let
      power = magnitude otherPlayer.velocity
      angle = angleBetweenPoints player.position otherPlayer.position |> (+) pi
      vx = cos angle |> (*) power
      vy = sin angle |> (*) power
  in
      { x = vx, y = vy }


--TODO: fix angle and power logic. the downwards deflection is too strong
deflectCeiling : Player -> Vector
deflectCeiling player =
  let
      power = magnitude player.velocity
      angle = angleBetweenPoints player.position { x = player.position.x, y = ceilingPosY+0.29 } |> (+) pi
      vx = cos angle |> (*) power
      vy = sin angle |> (*) power
  in
      { x = player.velocity.x, y = -player.velocity.y }
