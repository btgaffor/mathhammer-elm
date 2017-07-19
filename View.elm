module View exposing (..)

import Html exposing (Html, Attribute, div, input, text, button, h1, code, pre, br)
import Html.Attributes exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Form.Input as Input
import Bootstrap.Form as Form
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.Button as Button
import Bootstrap.Form.Select as Select
import Array as Array

import Model exposing (..)
import Update exposing (..)
import Distribution exposing (renderDistribution)

view : Model -> Html Msg
view model =
  div []
    [ CDN.stylesheet
    , h1 [] [ text "New Distribution" ]
    , Grid.containerFluid
      []
      [ Grid.simpleRow
          [ Grid.col
              [ Col.md4 ]
              [
                inputCard
                  Nothing
                  "General Options"
                  [ Form.group []
                      [ Form.label [] [ text "Iterations" ]
                      , Input.number
                        [ Input.value (toString model.iterations)
                        , Input.onInput ChangeIterations
                        ]
                      ]
                  , Form.group []
                      [ Form.label [] [ text "Target Wounds Per Model" ]
                      , Input.number [ Input.value (toString model.targetWounds) ]
                      ]
                  , Button.button [ Button.primary, Button.onClick AddWeapon ] [ text "Add Weapon" ]
                  ]

              ]
          ]
      , Grid.simpleRow
        (Array.indexedMap weaponInput model.weapons |> Array.toList)
      , Grid.simpleRow
          [ Grid.col
              [ Col.md12 ]
              [ Button.button [ Button.onClick RequestCalculation ] [ text "Perform Calculation" ]
              ]
          ]
      , Grid.simpleRow
          [ Grid.col
              [ Col.md12 ]
              [ codeBlock
                  (renderDistribution model.distribution)
              ]
          ]
      ]
    ]

weaponInput : Int -> Array.Array Roll -> Grid.Column Msg
weaponInput weaponIndex weapon =
  Grid.col
    [ Col.md4 ]
    (Array.indexedMap (rollInput weaponIndex weapon) weapon |> Array.toList)

rollInput : Int -> Array.Array Roll -> Int -> Roll -> Html Msg
rollInput weaponIndex weapon rollIndex roll =
  case roll of
    AttackRoll attackRecord ->
      inputCard
        (Just (weaponIndex, rollIndex))
        "Attacks"
        [ Form.group []
            [ Form.label [] [ text "Attacks (e.g. 1 or 2d6)" ]
            , Input.text
              [ Input.value attackRecord.attacks
              , Input.onInput (ChangeAttacks weaponIndex weapon rollIndex attackRecord)
              ]
            ]
        ]

    HitRoll hitRecord ->
      inputCard
        (Just (weaponIndex, rollIndex))
        "To Hit"
        [ Form.group []
            [ Form.label [] [ text "Ballistic/Weapon Skill" ]
            , Input.number
              [ Input.value (toString hitRecord.skill)
              , Input.onInput (ChangeSkill weaponIndex weapon rollIndex hitRecord)
              ]
            ]
        , Form.group []
            [ Form.label [] [ text "Reroll" ]
            , Select.select
                [ Select.onChange (ChangeHitReroll weaponIndex weapon rollIndex hitRecord) ]
                [ Select.item [ value "none" ] [ text "None" ]
                , Select.item [ value "all" ] [ text "All" ]
                , Select.item [ value "ones" ] [ text "Ones" ]
                ]
            ]
        ]

    WoundRoll woundRecord ->
      inputCard
        (Just (weaponIndex, rollIndex))
        "To Wound"
        [ Form.group []
            [ Form.label [] [ text "Strength" ]
            , Input.number
              [ Input.value (toString woundRecord.strength)
              , Input.onInput (ChangeStrength weaponIndex weapon rollIndex woundRecord)
              ]
            ]
        , Form.group []
            [ Form.label [] [ text "Toughness" ]
            , Input.number
              [ Input.value (toString woundRecord.toughness)
              , Input.onInput (ChangeToughness weaponIndex weapon rollIndex woundRecord)
              ]
            ]
        , Form.group []
            [ Form.label [] [ text "Reroll" ]
            , Select.select
                [ Select.onChange (ChangeWoundReroll weaponIndex weapon rollIndex woundRecord) ]
                [ Select.item [ value "none" ] [ text "None" ]
                , Select.item [ value "all" ] [ text "All" ]
                , Select.item [ value "ones" ] [ text "Ones" ]
                ]
            ]
        ]

    SaveRoll saveRecord ->
      inputCard
        (Just (weaponIndex, rollIndex))
        "Save"
        [ Form.group []
            [ Form.label [] [ text "Save" ]
            , Input.number
              [ Input.value (toString saveRecord.save)
              , Input.onInput (ChangeSave weaponIndex weapon rollIndex saveRecord)
              ]
            ]
        , Form.group []
            [ Form.label [] [ text "Armor Piercing" ]
            , Input.number
              [ Input.value (toString saveRecord.armorPiercing)
              , Input.onInput (ChangeArmorPiercing weaponIndex weapon rollIndex saveRecord)
              ]
            ]
        ]

    DamageRoll damageRecord ->
      inputCard
        (Just (weaponIndex, rollIndex))
        "Damage"
        [ Form.group []
            [ Form.label [] [ text "Damage (e.g. 1 or 2d6)" ]
            , Input.text
              [ Input.value damageRecord.damage
              , Input.onInput (ChangeDamage weaponIndex weapon rollIndex damageRecord)
              ]
            ]
        , Form.group []
            [ Form.label [] [ text "Additional Save (e.g. Feel No Pain)" ]
            , Input.number
              [ Input.value (toString damageRecord.additionalSave)
              , Input.onInput (ChangeAdditionalSave weaponIndex weapon rollIndex damageRecord)
              ]
            ]
        ]

inputCard : Maybe (Int, Int) -> String -> List (Html Msg) -> Html Msg
inputCard indexes header content =
  Grid.simpleRow
    [ Grid.col
        [ Col.md12 ]
        [ Card.config
            [ Card.attrs [ style [ ("margin-bottom", "20px") ] ] ]
            |> Card.headerH6 [] (List.append [ text header ] (removeButton indexes))
            |> Card.block [] [ Card.text [] content ]
            |> Card.view
        ]
    ]

removeButton : Maybe (Int, Int) -> List (Html msg)
removeButton index =
  case index of
    Just (weaponIndex, rollIndex) ->
      [ Button.button
          [ Button.danger
          , Button.small
          , Button.attrs [ style [ ("float", "right") ] ]
          ]
          [ text "X" ]
      ]
    Nothing -> []


codeBlock : List (Html Msg) -> Html Msg
codeBlock content =
  pre
    [ style
        [ ("padding", "5px")
        , ("color", "#333")
        , ("background-color", "#f5f5f5")
        , ("border", "1px solid #ccc")
        , ("border-radius", "5px")
        , ("margin-top", "10px")
        ]
    ]
    [ code [] content ]
