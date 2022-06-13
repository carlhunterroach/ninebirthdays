module BirthdayTest exposing (..)

import App exposing (..)
import Birthdays exposing (calculateBirthdays, compare, PlanetaryBirthday)
import Date
import Expect exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import Planet exposing (planets)
import Test exposing (..)
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


simple : Test
simple =
    describe "simple utility functions"
        [ test "9 birthdays returned" <|
            \_ ->
                let
                    birthdate =
                        Date.fromCalendarDate 1965 Sep 16

                    today =
                        Date.fromCalendarDate 2022 May 1
                in
                (List.length <| Birthdays.calculateBirthdays birthdate today)
                    |> Expect.equal 9
        , test "Mercury is first planet?" <|
            \_ ->
                let
                    birthdate =
                        Date.fromCalendarDate 1965 Sep 16

                    today =
                        Date.fromCalendarDate 2022 May 1

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
                        Date.fromCalendarDate 1965 Sep 16

                    today =
                        Date.fromCalendarDate 2022 May 1

                    days =
                        Birthdays.calculateBirthdays birthdate today

                    day =
                        Maybe.withDefault badPlanet
                            (List.head (List.filter (\m -> m.planetName == "Earth") days))
                in
                day.age |> Expect.equal 57
        , test "Earth birthday is same day of month/year?" <|
            \_ ->
                let
                    birthdate =
                        Date.fromCalendarDate 1865 Sep 16

                    today =
                        Date.fromCalendarDate 2022 May 1

                    days =
                        Birthdays.calculateBirthdays birthdate today

                    day =
                        Maybe.withDefault
                            badPlanet
                            (List.head (List.filter (\m -> m.planetName == "Earth") days))
                in
                day.earthDate |> Expect.equal (Date.fromCalendarDate 2022 Sep 16)
        , test "Test Date diff" <|
            \_ ->
                let
                    birthdate =
                        Date.fromCalendarDate 1865 Sep 16

                    today =
                        Date.fromCalendarDate 2022 May 5

                    years =
                        Date.diff Date.Years birthdate today

                    birthday =
                        Date.add Date.Years (years + 1) birthdate
                in
                birthday |> Expect.equal (Date.fromCalendarDate 2022 Sep 16)
        , test "check a birthdate on Jupiter" <|
            \_ ->
                let
                    planetaryBirthdays =
                        calculateBirthdays
                            (Date.fromCalendarDate 1965 Feb 13)
                            (Date.fromCalendarDate 2022 May 6)

                    jupiterish =
                        List.head (List.drop 4 planetaryBirthdays)
                in
                case jupiterish of
                    Just jupiter ->
                        Date.compare jupiter.earthDate (Date.fromCalendarDate 2024 Jun 5) |> Expect.equal EQ

                    _ ->
                        Debug.todo "Never can reach here"
        ]
