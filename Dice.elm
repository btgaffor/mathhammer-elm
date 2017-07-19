module Dice exposing (maxXdY, rollXdY, rollRound)

import Array
import Random

maxXdY : String -> Int
maxXdY dice =
  let
    (numberDice, sides) = splitDice dice
  in
    numberDice * sides

-- seed has to be passed in an out to make sure we're
-- only using each seed increment once
rollXdY : String -> Random.Seed -> (Int, Random.Seed)
rollXdY dice seed =
  let
    (numberDice, sides) = splitDice dice
  in
    Array.foldl
      (\n (memo, seed) ->
         let
           (randomNumber, newSeed) = Random.step (Random.int  1 sides) seed
         in
          (memo + randomNumber, newSeed)
      )
      (0, seed)
      (Array.repeat numberDice 0)

rollD6 : Random.Seed -> (Int, Random.Seed)
rollD6 originalSeed =
  Random.step (Random.int 1 6) originalSeed

splitDice : String -> (Int, Int)
splitDice dice =
  let
    indexes = String.indexes "d" dice
  in
    case indexes of
      [] ->
        case (String.toInt dice) of
          Ok num -> (num, 1)
          Err error -> (0, 1)
      [index] ->
        let
          result =
            Result.map2
              (\numberDice sides -> (numberDice, sides))
              (String.toInt (String.left index dice))
              (String.toInt (String.right ((String.length dice) - index - 1) dice))
        in
          case result of
            Ok split -> split
            Err error -> (0, 1) -- TODO: error message?
      _ -> (0, 1) -- TODO: maybe some kind of error message?

rollRound : Int -> (Int -> Bool) -> String -> Random.Seed -> (Int, Random.Seed)
rollRound numberOfDice rollSucceeded reroll originalSeed =
  List.foldl
    (\n (memo, seed) ->
       let
         (pass, newSeed) = rollWithReroll rollSucceeded reroll seed
       in
         case pass of
           True -> (memo + 1, newSeed)
           False -> (memo, newSeed)
    )
    (0, originalSeed)
    (List.range 1 numberOfDice)

rollWithReroll : (Int -> Bool) -> String -> Random.Seed ->  (Bool, Random.Seed)
rollWithReroll rollSucceeded reroll originalSeed =
  let
    (roll, newSeed) = rollD6 originalSeed
    pass = rollSucceeded roll
  in
    case pass of
      True -> (True, newSeed)
      False ->
        if (roll == 1 && reroll == "ones") || reroll == "all" then
          rollWithReroll rollSucceeded "none" newSeed
        else
          (False, newSeed)
