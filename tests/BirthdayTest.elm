module BirthdayTest exposing (..)

import App exposing (..)
import Birthdays exposing (PlanetaryBirthday, calculateBirthdays)
import Date
import Expect
import Test exposing (Test, describe, test)
import Time exposing (Month(..))


fallbackDate : Date.Date
fallbackDate =
    Date.fromCalendarDate 1 Jan 1


badPlanet : PlanetaryBirthday
badPlanet =
    { planetName = "BadPlanet"
    , age = 99999
    , earthDate = fallbackDate
    , todayOnEarth = fallbackDate
    }


notMercury : PlanetaryBirthday
notMercury =
    { planetName = "NotMercury"
    , age = 21
    , earthDate = fallbackDate
    , todayOnEarth = fallbackDate
    }


is_earth : PlanetaryBirthday -> Bool
is_earth planetary_birthday =
    planetary_birthday.planetName == "Earth"


is_jupiter : PlanetaryBirthday -> Bool
is_jupiter planetary_birthday =
    planetary_birthday.planetName == "Jupiter"


simple : Test
simple =
    describe "simple utility functions"
        [ test "9 birthdays returned" <|
            \_ ->
                let
                    birthdate =
                        Date.fromCalendarDate 2000 Feb 28

                    today =
                        Date.fromCalendarDate 2022 May 1
                in
                (List.length <| Birthdays.calculateBirthdays birthdate today)
                    |> Expect.equal 9
        , test "Mercury is first planet of birthdays?" <|
            \_ ->
                let
                    birthdate =
                        Date.fromCalendarDate 2000 Mar 20

                    today =
                        Date.fromCalendarDate 2022 Jul 27

                    days =
                        Birthdays.calculateBirthdays birthdate today

                    day =
                        Maybe.withDefault notMercury (List.head days)
                in
                day.planetName |> Expect.equal "Mercury"
        ]


complex : Test
complex =
    describe "date functions"
        [ test "Earth birthday is correct age?" <|
            \_ ->
                let
                    birthdate =
                        Date.fromCalendarDate 2000 Feb 1

                    today =
                        Date.fromCalendarDate 2022 Jul 27

                    days =
                        Birthdays.calculateBirthdays birthdate today

                    day =
                        Maybe.withDefault badPlanet
                            (List.head (List.filter is_earth days))
                in
                day.age |> Expect.equal 23
        , test "Earth birthday is same day of month/year?" <|
            \_ ->
                let
                    birthdate =
                        Date.fromCalendarDate 1805 Jan 31

                    today =
                        Date.fromCalendarDate 2022 Jul 27

                    days =
                        Birthdays.calculateBirthdays birthdate today

                    day =
                        Maybe.withDefault
                            badPlanet
                            (List.head (List.filter is_earth days))
                in
                day.earthDate
                    |> Expect.equal (Date.fromCalendarDate 2023 Jan 31)
        , test "check a birthdate on Jupiter" <|
            \_ ->
                let
                    planetaryBirthdays =
                        calculateBirthdays
                            (Date.fromCalendarDate 1965 Feb 13)
                            (Date.fromCalendarDate 2022 May 6)

                    jupiterish =
                        List.head (List.filter is_jupiter planetaryBirthdays)
                in
                case jupiterish of
                    Just jupiter ->
                        Date.compare jupiter.earthDate
                            (Date.fromCalendarDate 2024 Jun 5)
                            |> Expect.equal EQ

                    _ ->
                        Debug.todo "Never can reach here"
        ]
