module Update exposing (..)

import Set exposing (Set)
import Char
import Time exposing (Time)
import Keyboard exposing (KeyCode)

import Model exposing (..)
import Model.Ui exposing (..)
import Model.Scene exposing (..)
import Model.Geometry exposing (..)
import Subscription exposing (..)

import Debug exposing (log)


update : Msg -> Model -> (Model, Cmd Msg)
update action ({ui,scene} as model) =
  case action of
    ResizeWindow dimensions ->
      ({ model | ui = { ui | windowSize = dimensions } }, Cmd.none)

    Tick delta ->
      let
          player1 = scene.player1 |> steerAndGravity delta ui
          player2 = scene.player2 |> steerAndGravity delta ui
          round = scene.round
          (player1_, player2_) = handleCollisions player1 player2
          player1__ = player1_ |> movePlayer delta
          player2__ = player2_ |> movePlayer delta
          hasAnyPlayerFallen = hasFallen player1 || hasFallen player2
          isRoundOver = hasAnyPlayerFallen && round.touchdownTime > 1400
          (player1___, player2___) = applyScores player1__ player2__ isRoundOver
          isGameOver = player1___.score>=winScore || player2___.score>=winScore
          (round_, screen_) =
            if isGameOver then
               (round, GameoverScreen)
            else if isRoundOver then
               (newRound, PlayScreen)
            else if hasAnyPlayerFallen then
              ({ round | touchdownTime = round.touchdownTime + delta }, PlayScreen)
            else
              (round, PlayScreen)
          scene_ = { scene | player1 = player1___
                           , player2 = player2___
                           , round = round_ }
          ui_ = { ui | screen = screen_ }
      in
          ({ model | scene = scene_, ui = ui_ }, Cmd.none)

    KeyChange pressed keycode ->
      (handleKeyChange pressed keycode model, Cmd.none)

    StartGame ->
      (freshGame ui, Cmd.none)

    TimeSecond _ ->
      ({ model | secondsPassed = model.secondsPassed+1 }, Cmd.none)

    NoOp ->
      (model, Cmd.none)


applyScores : Player -> Player -> Bool -> (Player,Player)
applyScores player1 player2 isRoundOver =
  if isRoundOver then
    let
        pointsForPlayer1 = if hasFallen player2 then 1 else 0
        pointsForPlayer2 = if hasFallen player1 then 1 else 0
    in
        (payAndReset player1 pointsForPlayer1
        ,payAndReset player2 pointsForPlayer2)
  else
    (player1, player2)


payAndReset : Player -> Int -> Player
payAndReset player additionalPoints =
  let
      position_ = Vector player.homePosX playerHomePosY
      velocity_ = Vector 0 0
  in
      { player | score = player.score + additionalPoints
               , position = position_
               , velocity = velocity_ }


hasFallen : Player -> Bool
hasFallen player =
  player.position.y > 1 + playerRadius*2


steerAndGravity : Time -> Ui -> Player -> Player
steerAndGravity delta {pressedKeys} ({velocity} as player) =
  let
      directionX = if keyPressed player.leftKey pressedKeys && not (keyPressed player.rightKey pressedKeys) then
                     -1
                   else if keyPressed player.rightKey pressedKeys && not (keyPressed player.leftKey pressedKeys) then
                     1
                   else
                     0
      ax = directionX * 0.0000019
      vx_ = 
        if not (player.position.y==floorPosY) then 
          velocity.x + ax*delta |> friction delta
        else (velocity.x + ax*delta) / 1.05 |> friction delta
      vy_ = velocity.y |> (gravity delta)
      velocity_ = { velocity | x = vx_
                             , y = vy_ }
  in
     { player | velocity = velocity_ }


handleCollisions : Player -> Player -> (Player,Player)
handleCollisions player1 player2 =
  if playersOverlap player1 player2 then
    bounceOffEachOther player1 player2
  else if playerHitCeiling player1 then
    (bounceOffCeiling player1, player2)
  else if playerHitCeiling player2 then
    (bounceOffCeiling player2, player1)
  else
    (player1, player2)


bounceOffEachOther : Player -> Player -> (Player,Player)
bounceOffEachOther player1 player2 =
  let
      v1 = deflect player1 player2
      v2 = deflect player2 player1
      player1_ = { player1 | velocity = v1 }
      player2_ = { player2 | velocity = v2 }
  in
      (player1_, player2_)


bounceOffCeiling : Player -> Player
bounceOffCeiling player =
  let
      v1 = deflectCeiling player
      player_ = { player | velocity = v1 }
  in
      player_


movePlayer : Time -> Player -> Player
movePlayer delta ({velocity,position} as player) =
  let
      vx = velocity.x
      vy = velocity.y
      airborne = position.y + vy*delta < floorPosY
      stepFn = 
        if airborne then
          glide
        else if airborne && inBounds position.x then
          glide
        else if inBounds position.x then
          walk
        else
          fall
      (x_, y_, vx_, vy_) = stepFn delta position.x position.y vx vy
      position_ = { position | x = x_
                             , y = y_ }
      velocity_ = { velocity | x = vx_
                             , y = vy_ }
  in
      { player | position = position_
               , velocity = velocity_ }



glide : Time -> Float -> Float -> Float -> Float -> (Float,Float,Float,Float)
glide delta x y vx vy =
  (x+vx*delta, y+vy*delta, vx, vy)


walk : Time -> Float -> Float -> Float -> Float -> (Float,Float,Float,Float)
walk delta x y vx vy =
  let
      x_ = x + vx*delta
      y_ = floorPosY
      vy = 0
  in
      (x_, y_, vx, vy)


fall : Time -> Float -> Float -> Float -> Float -> (Float,Float,Float,Float)
fall delta x y vx vy =
  let
      y_ = y + vy*delta
      x_ = x + vx*delta
      isLeftSide = x<0.5
      x__ = if y_<floorPosY+playerRadius then
             rollOffEdge x_ y_ isLeftSide
           else
             keepOutOfBounds x_
  in
      (x__, y_, vx, vy)


inBounds : Float -> Bool
inBounds x =
  x>=floorPosX && x<=floorRightEdgeX


friction : Time -> Float -> Float
friction delta vx =
  vx / (1 + 0.0018*delta)


jump : Player -> Player
jump ({position,velocity} as player) =
  let
      vy = 
        if velocity.y < -0.001 then 
          velocity.y
        else 
          (velocity.y) - 0.0009
      velocity_ = { velocity | y = vy }
  in
      { player | velocity = velocity_ }


gravity : Time -> Float -> Float
gravity delta vy =
  vy + 0.000003 * delta


rollOffEdge : Float -> Float -> Bool -> Float
rollOffEdge x y isLeftSide =
  let
      edgePosX = if isLeftSide then floorPosX else floorRightEdgeX
      increment = 0.003 * (if isLeftSide then -1 else 1)
  in
      if distance (x,y-playerRadius) (edgePosX,floorPosY) > playerRadius then
        x
      else
        rollOffEdge (x + increment) y isLeftSide


keepOutOfBounds : Float -> Float
keepOutOfBounds x =
  if x<0.5 then
     min x (floorPosX-playerRadius)
  else
     max x (floorRightEdgeX+playerRadius)


handleKeyChange : Bool -> KeyCode -> Model -> Model
handleKeyChange pressed keycode ({scene,ui} as model) =
  let
      fn = if pressed then Set.insert else Set.remove
      pressedKeys_ = fn keycode ui.pressedKeys
  in
      case ui.screen of
        PlayScreen ->
          let
              ui_ = { ui | pressedKeys = pressedKeys_ }
              justPressed keycode = freshKeyPress keycode ui.pressedKeys pressedKeys_
              maybeJump player = 
                if justPressed player.jumpKey then
                  jump player
                else
                  player
              player1_ = maybeJump scene.player1
              player2_ = maybeJump scene.player2
              scene_ = { scene |  player1 = player1_, player2 = player2_ }
          in
              { model | ui = ui_, scene = scene_ }

        GameoverScreen ->
          if freshKeyPress (Char.toCode ' ') ui.pressedKeys pressedKeys_ then
            freshGame ui
          else
            model

        _ ->
          model


freshKeyPress : KeyCode -> Set KeyCode -> Set KeyCode -> Bool
freshKeyPress keycode previouslyPressedKeys currentlyPressedKeys =
  let
      pressed = keyPressed keycode
  in
      pressed currentlyPressedKeys && not (pressed previouslyPressedKeys)
