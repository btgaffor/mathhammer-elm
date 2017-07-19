module Update exposing (..)

import Array as Array
import Random
import Model exposing (..)
import Distribution

model : Model
model =
  {
    iterations = 10000,
    targetWounds = 1,
    weapons = Array.fromList [defaultWeapon],
    distribution = Nothing
  }

init: (Model, Cmd Msg)
init =
  (model, Cmd.none)

defaultWeapon : Array.Array Roll
defaultWeapon =
  Array.fromList
    [
      AttackRoll { attacks = "1" },
      HitRoll { skill = 0, reroll = "none" },
      WoundRoll { strength = 0, toughness = 0, reroll = "none" },
      SaveRoll { save = 0, armorPiercing = 0 },
      DamageRoll { damage = "1", additionalSave = 7 }
    ]

-- The actual data record is already known by this time because we're switching
-- on the roll type when rendering. This means we can't even use the wrong action
-- from here without a type error
type Msg = AddWeapon
         | RequestCalculation
         | PerformCalculation Int
         | ChangeIterations String
         | ChangeAttacks Int (Array.Array Roll) Int AttackRecord String
         | ChangeSkill Int (Array.Array Roll) Int HitRecord String
         | ChangeHitReroll Int (Array.Array Roll) Int HitRecord String
         | ChangeStrength Int (Array.Array Roll) Int WoundRecord String
         | ChangeToughness Int (Array.Array Roll) Int WoundRecord String
         | ChangeWoundReroll Int (Array.Array Roll) Int WoundRecord String
         | ChangeSave Int (Array.Array Roll) Int SaveRecord String
         | ChangeArmorPiercing Int (Array.Array Roll) Int SaveRecord String
         | ChangeDamage Int (Array.Array Roll) Int DamageRecord String
         | ChangeAdditionalSave Int (Array.Array Roll) Int DamageRecord String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RequestCalculation ->
      (model, Random.generate PerformCalculation (Random.int Random.minInt Random.maxInt))

    PerformCalculation seed ->
      ({ model | distribution = Distribution.distributionData model seed |> Just }, Cmd.none)

    AddWeapon ->
      ({ model | weapons = Array.push defaultWeapon model.weapons }, Cmd.none)

    ChangeIterations newIterations ->
      case (String.toInt newIterations) of
        Ok newIterations ->
          ({ model | iterations = newIterations}, Cmd.none)
        Err error -> (model, Cmd.none)

    ChangeAttacks weaponIndex weapon rollIndex attackRecord newAttacks ->
      let
        newAttackRecord = { attackRecord | attacks = newAttacks }
        newWeapon = Array.set rollIndex (AttackRoll newAttackRecord) weapon
        newWeapons = Array.set weaponIndex newWeapon model.weapons
      in
        ({ model | weapons = newWeapons }, Cmd.none)

    ChangeSkill weaponIndex weapon rollIndex hitRecord newSkill ->
      case String.toInt newSkill of
        Ok newSkill ->
          let
            newHitRecord = { hitRecord | skill = newSkill }
            newWeapon = Array.set rollIndex (HitRoll newHitRecord) weapon
            newWeapons = Array.set weaponIndex newWeapon model.weapons
          in
            ({ model | weapons = newWeapons }, Cmd.none)
        Err error -> (model, Cmd.none)

    ChangeHitReroll weaponIndex weapon rollIndex hitRecord newReroll ->
      let
        newHitRecord = { hitRecord | reroll = newReroll }
        newWeapon = Array.set rollIndex (HitRoll newHitRecord) weapon
        newWeapons = Array.set weaponIndex newWeapon model.weapons
      in
        ({ model | weapons = newWeapons }, Cmd.none)

    ChangeStrength weaponIndex weapon rollIndex woundRecord newStrength ->
      case String.toInt newStrength of
        Ok newStrength ->
          let
            newWoundRecord = { woundRecord | strength = newStrength }
            newWeapon = Array.set rollIndex (WoundRoll newWoundRecord) weapon
            newWeapons = Array.set weaponIndex newWeapon model.weapons
          in
            ({ model | weapons = newWeapons }, Cmd.none)
        Err error -> (model, Cmd.none)

    ChangeToughness weaponIndex weapon rollIndex woundRecord newToughness ->
      case String.toInt newToughness of
        Ok newToughness ->
          let
            newWoundRecord = { woundRecord | toughness = newToughness }
            newWeapon = Array.set rollIndex (WoundRoll newWoundRecord) weapon
            newWeapons = Array.set weaponIndex newWeapon model.weapons
          in
            ({ model | weapons = newWeapons }, Cmd.none)
        Err error -> (model, Cmd.none)

    ChangeWoundReroll weaponIndex weapon rollIndex woundRecord newReroll ->
      let
        newWoundRecord = { woundRecord | reroll = newReroll }
        newWeapon = Array.set rollIndex (WoundRoll newWoundRecord) weapon
        newWeapons = Array.set weaponIndex newWeapon model.weapons
      in
        ({ model | weapons = newWeapons }, Cmd.none)

    ChangeSave weaponIndex weapon rollIndex saveRecord newSave ->
      case String.toInt newSave of
        Ok newSave ->
          let
            newSaveRecord = { saveRecord | save = newSave }
            newWeapon = Array.set rollIndex (SaveRoll newSaveRecord) weapon
            newWeapons = Array.set weaponIndex newWeapon model.weapons
          in
            ({ model | weapons = newWeapons }, Cmd.none)
        Err error -> (model, Cmd.none)

    ChangeArmorPiercing weaponIndex weapon rollIndex saveRecord newArmorPiercing ->
      case String.toInt newArmorPiercing of
        Ok newArmorPiercing ->
          let
            newSaveRecord = { saveRecord | armorPiercing = newArmorPiercing }
            newWeapon = Array.set rollIndex (SaveRoll newSaveRecord) weapon
            newWeapons = Array.set weaponIndex newWeapon model.weapons
          in
            ({ model | weapons = newWeapons }, Cmd.none)
        Err error -> (model, Cmd.none)

    ChangeDamage weaponIndex weapon rollIndex damageRecord newDamage ->
      let
        newDamageRecord = { damageRecord | damage = newDamage }
        newWeapon = Array.set rollIndex (DamageRoll newDamageRecord) weapon
        newWeapons = Array.set weaponIndex newWeapon model.weapons
      in
        ({ model | weapons = newWeapons }, Cmd.none)

    ChangeAdditionalSave weaponIndex weapon rollIndex damageRecord newAdditionalSave ->
      case String.toInt newAdditionalSave of
        Ok newAdditionalSave ->
          let
            newDamageRecord = { damageRecord | additionalSave = newAdditionalSave }
            newWeapon = Array.set rollIndex (DamageRoll newDamageRecord) weapon
            newWeapons = Array.set weaponIndex newWeapon model.weapons
          in
            ({ model | weapons = newWeapons }, Cmd.none)
        Err error -> (model, Cmd.none)
