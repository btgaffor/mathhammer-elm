module Model exposing (..)

import Array

type alias AttackRecord =
  {
    attacks: String
  }

type alias HitRecord =
  {
    skill: Int,
    reroll: String
  }

type alias WoundRecord =
  {
    strength: Int,
    toughness: Int,
    reroll: String
  }

type alias SaveRecord =
  {
    save: Int,
    armorPiercing: Int
  }

type alias DamageRecord =
  {
    damage: String,
    additionalSave: Int
  }

type Roll =
      AttackRoll AttackRecord
    | HitRoll HitRecord
    | WoundRoll WoundRecord
    | SaveRoll SaveRecord
    | DamageRoll DamageRecord

type alias Weapon = Array.Array (Roll)

type alias DistributionRow =
  {
    thisPercent: Float,
    totalPercent: Float
  }

type alias DistributionTable = Array.Array DistributionRow

type alias Model =
  {
    iterations: Int,
    targetWounds: Int,
    weapons: Array.Array (Array.Array Roll),
    distribution: Maybe (DistributionTable)
  }
