module Birthdays exposing
    ( Birthdate
    , PlanetaryBirthday
    , Today
    , calculateBirthdays
    , compare
    )

{-| birthdays and ages on solar system planets

    Given a birthdate, a planet and today's date
        calculate a birthday date and age

-}

import Date exposing (Date)
import Planet exposing (OrbitDays, Planet(..), planets)


type alias Years =
    Int


type alias Today =
    Date


type alias Birthdate =
    Date


type alias Birthday =
    Date


type alias PlanetaryBirthday =
    { planetName : String
    , age : Years
    , earthDate : Birthday
    , todayOnEarth : Today
    }


toAge : Planet -> Birthdate -> Today -> Int
toAge planet birthdate today =
    case planet of
        Earth _ ->
            nextEarthAge birthdate today

        Alien _ orbitDays ->
            nextAlienAge birthdate today orbitDays


toBirthday : Planet -> Birthdate -> Today -> Birthday
toBirthday planet birthdate today =
    case planet of
        Earth _ ->
            nextEarthBirthday birthdate today

        Alien _ orbitDays ->
            nextAlienBirthday birthdate today orbitDays


daysSinceBirthZeroIfInFuture : Birthdate -> Today -> Int
daysSinceBirthZeroIfInFuture birthdate today =
    Basics.max 0 (Date.diff Date.Days birthdate today)


yearsSinceBirthZeroIfInFuture : Birthdate -> Today -> Int
yearsSinceBirthZeroIfInFuture birthdate today =
    Basics.max 0 (Date.diff Date.Years birthdate today)


nextEarthAge : Birthdate -> Today -> Int
nextEarthAge birthdate today =
    yearsSinceBirthZeroIfInFuture birthdate today + 1


nextAlienAge : Birthdate -> Today -> OrbitDays -> Int
nextAlienAge birthdate today orbit =
    let
        ageToday =
            toFloat (daysSinceBirthZeroIfInFuture birthdate today)
                / orbit
    in
    if birthdate == today || floor ageToday == 0 then
        1

    else
        ceiling ageToday


nextEarthBirthday : Birthdate -> Today -> Birthday
nextEarthBirthday birthdate today =
    Date.add Date.Years
        (yearsSinceBirthZeroIfInFuture birthdate today + 1)
        birthdate


daysToNextAlienBirthday : Float -> Int -> Int
daysToNextAlienBirthday orbit daysSinceBirth =
    floor
        (toFloat
            (floor (toFloat daysSinceBirth / orbit) + 1)
            * orbit
        )


nextAlienBirthday : Birthdate -> Today -> OrbitDays -> Birthday
nextAlienBirthday birthdate today orbit =
    let
        daysToNextBirthday =
            daysSinceBirthZeroIfInFuture birthdate today
                |> daysToNextAlienBirthday orbit
    in
    Date.add Date.Days daysToNextBirthday birthdate


calculateBirthday : Birthdate -> Today -> Planet -> PlanetaryBirthday
calculateBirthday birthdate today planet =
    { planetName = Planet.toName planet
    , age = toAge planet birthdate today
    , earthDate = toBirthday planet birthdate today
    , todayOnEarth = today -- helps identify birthday's occuring today
    }


calculateBirthdays : Birthdate -> Today -> List PlanetaryBirthday
calculateBirthdays birthdate today =
    List.map (calculateBirthday birthdate today) planets


compare : PlanetaryBirthday -> PlanetaryBirthday -> Order
compare a b =
    Date.compare a.earthDate b.earthDate
