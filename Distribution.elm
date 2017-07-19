module Distribution exposing (..)

import Html exposing (Html, text, br)
import Formatting exposing (..)
import String exposing (repeat)
import Array
import Random

import Model exposing (..)
import Dice

---------------
-- Rendering --
---------------

renderDistribution : Maybe (Array.Array DistributionRow) -> List (Html msg)
renderDistribution distribution =
  case distribution of
    Just distribution ->
      List.concat
        (List.indexedMap
          (row <| Array.length distribution - 1)
          (Array.toList distribution)
        )
    Nothing -> []

row : Int -> Int -> DistributionRow -> List (Html msg)
row maxIndex index distributionRow =
  let
    { thisPercent, totalPercent } = distributionRow
  in
    [ text (formatRow (maxIndex - index) thisPercent totalPercent thisPercent)
    , br [] []
    ]

formatRow : Int -> Float -> Float -> Float -> String
formatRow =
  let
    count = padLeft 2 '0' int
    thisPercent = padLeft 6 ' ' (roundTo 2)
    totalPercent = padLeft 6 ' ' (roundTo 2)
  in
    print (count <> s " " <> thisPercent <> s "% " <> totalPercent <> s "% " <> hashes)

hashes : Format r (Float -> r)
hashes =
    Format
        (\callback count ->
            callback <| (repeat (floor count) "#")
        )

-----------------
-- Calculating --
-----------------

distributionData : Model -> Int -> DistributionTable
distributionData model initialSeed =
  let
    seed = Random.initialSeed initialSeed
    maxDamage = calculateMaxDamage model
    (distribution, newSeed) = calculateDistribution model maxDamage seed
    percentages = calculatePercentages distribution model.iterations
  in
    percentages

calculateMaxDamage : Model -> Int
calculateMaxDamage model =
  Array.foldl
    (+) 0
    (Array.map calculateWeaponMaxDamage model.weapons)

calculateWeaponMaxDamage : Weapon -> Int
calculateWeaponMaxDamage weapon =
  Array.foldl
    (\roll memo ->
       case roll of
         AttackRoll { attacks } -> Dice.maxXdY attacks
         DamageRoll { damage } -> memo * (Dice.maxXdY damage)
         _ -> memo
    )
    0
    weapon

calculateDistribution : Model -> Int -> Random.Seed -> (Array.Array (Int), Random.Seed)
calculateDistribution model maxDamage originalSeed =
  List.foldl
    (\n (memo, seed) ->
       let
         (damage, newSeed) = calculateTotalDamage model.weapons seed
         currentCount = Array.get damage memo
       in
         case currentCount of
           Just currentCount -> (Array.set damage (currentCount + 1) memo, newSeed)
           Nothing -> (memo, newSeed) -- TODO: display error? shouldn't happen, though
    )
    ((Array.repeat (maxDamage + 1) 0), originalSeed)
    (List.range 1 model.iterations)

calculateTotalDamage : Array.Array (Weapon) -> Random.Seed -> (Int, Random.Seed)
calculateTotalDamage weapons originalSeed =
  Array.foldl
    (\weapon (memo, seed) ->
      let
        (damage, newSeed) = calculateWeaponDamage weapon seed
      in
        (memo + damage, newSeed)
    )
    (0, originalSeed)
    weapons

calculateWeaponDamage : Weapon -> Random.Seed -> (Int, Random.Seed)
calculateWeaponDamage weapon seed =
  Array.foldl
    (\roll (memo, seed) ->
       case roll of
         AttackRoll { attacks } ->
           -- simply roll to find out how many attacks there are
           Dice.rollXdY attacks seed
         HitRoll { skill, reroll } ->
           -- roll them all and keep the ones that pass
           Dice.rollRound memo (\r -> r >= skill) reroll seed
         WoundRoll { strength, toughness, reroll } ->
           let
             rollNeeded =
               if strength >= 2 * toughness then
                 2
               else if strength > toughness then
                 3
               else if (strength * 2) <= toughness then
                 6
               else if strength < toughness then
                 5
               else
                 4
           in
             -- roll them all and keep the ones that pass
             Dice.rollRound memo (\r -> r >= rollNeeded) reroll seed

         SaveRoll { save, armorPiercing } ->
           -- roll them all and keep the ones that pass
           Dice.rollRound memo (\r -> r < (save - armorPiercing)) "none" seed
         DamageRoll { damage } ->
           -- roll for each attack and total all of the damages
           List.foldl
             (\n (memo, previousSeed) ->
               let
                 (roll, newSeed) = Dice.rollXdY damage previousSeed
               in
                 (memo + roll, newSeed)
             )
             (0, seed)
             (List.range 1 memo)
         -- AdditionalSaveRoll { additionalSave } ->
         --   -- roll them all and keep the ones that pass
         --   Dice.rollRound memo (\r -> r < additionalSave) "none" seed
    )
    (0, seed)
    weapon

calculatePercentages : Array.Array (Int) -> Int -> DistributionTable
calculatePercentages counts iterations =
  let
    (distribution, totalPercent) =
      Array.foldr
        (\count (memo, total) ->
          let
            thisPercent = (toFloat count) / (toFloat iterations) * 100
            newTotal = total + thisPercent
            newArray = Array.push { thisPercent = thisPercent, totalPercent = newTotal } memo
          in
            (newArray, newTotal)
        )
        (Array.empty, 0)
        counts
  in
    distribution
