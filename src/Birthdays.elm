module Birthdays exposing
    ( Birthdate
    , PlanetaryBirthday
    , Today
    , calculate_birthdays
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
    { planet_name : String
    , age : Years
    , earth_date : Birthday
    , today_on_earth : Today
    }



-- FUNCTIONS


toAge : Planet -> Birthdate -> Today -> Int
toAge planet birthdate today =
    case planet of
        Earth _ ->
            next_earth_age birthdate today

        Alien _ orbit_days ->
            next_alien_age birthdate today orbit_days


toBirthday : Planet -> Birthdate -> Today -> Birthday
toBirthday planet birthdate today =
    case planet of
        Earth _ ->
            next_earth_birthday birthdate today

        Alien _ orbit_days ->
            next_alien_birthday birthdate today orbit_days


days_since_birth_zero_if_in_future : Birthdate -> Today -> Int
days_since_birth_zero_if_in_future birthdate today =
    Basics.max 0 (Date.diff Date.Days birthdate today) + 1


years_since_birth_zero_if_in_future : Birthdate -> Today -> Int
years_since_birth_zero_if_in_future birthdate today =
    Basics.max 0 (Date.diff Date.Years birthdate today)



-- FUNCTIONS


next_earth_age : Birthdate -> Today -> Int
next_earth_age birthdate today =
    years_since_birth_zero_if_in_future birthdate today + 1


next_alien_age : Birthdate -> Today -> OrbitDays -> Int
next_alien_age birthdate today orbit =
    let
        earth_days_since_birth =
            days_since_birth_zero_if_in_future birthdate today
    in
    ((toFloat earth_days_since_birth / orbit) |> floor) + 1


next_earth_birthday : Birthdate -> Today -> Birthday
next_earth_birthday birthdate today =
    let
        years =
            years_since_birth_zero_if_in_future birthdate today
    in
    Date.add Date.Years (years + 1) birthdate


next_alien_birthday : Birthdate -> Today -> OrbitDays -> Birthday
next_alien_birthday birthdate today orbit =
    let
        days_since_birth =
            days_since_birth_zero_if_in_future birthdate today

        next_age =
            ((toFloat days_since_birth / orbit) |> floor) + 1

        days_to_next_birthday =
            (toFloat next_age * orbit) |> floor
    in
    Date.add Date.Days days_to_next_birthday birthdate


calculate_birthday : Birthdate -> Today -> Planet -> PlanetaryBirthday
calculate_birthday birthdate today planet =
    { planet_name = Planet.toName planet
    , age = toAge planet birthdate today
    , earth_date = toBirthday planet birthdate today
    , today_on_earth = today -- identify birthday's occuring today
    }


calculate_birthdays : Birthdate -> Today -> List PlanetaryBirthday
calculate_birthdays birthdate today =
    List.map (calculate_birthday birthdate today) planets


compare : PlanetaryBirthday -> PlanetaryBirthday -> Order
compare a b =
    Date.compare a.earth_date b.earth_date
