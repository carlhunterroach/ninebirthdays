module Birthdays exposing
    ( Birthdate
    , PlanetaryBirthday
    , Today
    , calculateBirthdays
    , compare
    )

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



-- FUNCTIONS


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
    Basics.max 0 (Date.diff Date.Days birthdate today) + 1


yearsSinceBirthZeroIfInFuture : Birthdate -> Today -> Int
yearsSinceBirthZeroIfInFuture birthdate today =
    Basics.max 0 (Date.diff Date.Years birthdate today)



-- FUNCTIONS


nextEarthAge : Birthdate -> Today -> Int
nextEarthAge birthdate today =
    yearsSinceBirthZeroIfInFuture birthdate today + 1


nextAlienAge : Birthdate -> Today -> OrbitDays -> Int
nextAlienAge birthdate today orbit =
    ((toFloat (daysSinceBirthZeroIfInFuture birthdate today) / orbit) |> floor) + 1


nextEarthBirthday : Birthdate -> Today -> Birthday
nextEarthBirthday birthdate today =
    Date.add Date.Years (yearsSinceBirthZeroIfInFuture birthdate today + 1) birthdate


ageAtNextBirthday : Float -> Int -> Int
ageAtNextBirthday orbit daysSinceBirth =
    ((toFloat daysSinceBirth / orbit) |> floor) + 1


daysToNextBirthday : Float -> Int -> Int
daysToNextBirthday orbit nextBirthdayAge =
    (toFloat nextBirthdayAge * orbit) |> floor


nextAlienBirthday : Birthdate -> Today -> OrbitDays -> Birthday
nextAlienBirthday birthdate today orbit =
    Date.add Date.Days
        (daysSinceBirthZeroIfInFuture birthdate today
            |> ageAtNextBirthday orbit
            |> daysToNextBirthday orbit
        )
        birthdate


calculate_birthday : Birthdate -> Today -> Planet -> PlanetaryBirthday
calculate_birthday birthdate today planet =
    { planetName = Planet.toName planet
    , age = toAge planet birthdate today
    , earthDate = toBirthday planet birthdate today
    , todayOnEarth = today -- identify birthday's occuring today
    }


calculateBirthdays : Birthdate -> Today -> List PlanetaryBirthday
calculateBirthdays birthdate today =
    List.map (calculate_birthday birthdate today) planets


compare : PlanetaryBirthday -> PlanetaryBirthday -> Order
compare a b =
    Date.compare a.earthDate b.earthDate
