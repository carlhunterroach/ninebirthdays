module BirthdayTest exposing (..)

import App exposing (..)
import Birthdays exposing (calculate_birthdays, compare, PlanetaryBirthday)
import Date
import Expect exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import Planet exposing (planets)
import Test exposing (..)
import Time exposing (Month(..))


fallback_date : Date.Date
fallback_date =
    Date.fromCalendarDate 1 Jan 1


bad_planet : PlanetaryBirthday
bad_planet =
    { planet_name = "BadPlanet"
    , age = 99999
    , earth_date = fallback_date
    , today_on_earth = fallback_date
    }


not_mercury : PlanetaryBirthday
not_mercury =
    { planet_name = "NotMercury"
    , age = 21
    , earth_date = fallback_date
    , today_on_earth = fallback_date
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
                (List.length <| Birthdays.calculate_birthdays birthdate today)
                    |> Expect.equal 9
        , test "Mercury is first planet?" <|
            \_ ->
                let
                    birthdate =
                        Date.fromCalendarDate 1965 Sep 16

                    today =
                        Date.fromCalendarDate 2022 May 1

                    days =
                        Birthdays.calculate_birthdays birthdate today

                    day =
                        Maybe.withDefault not_mercury (List.head days)
                in
                day.planet_name |> Expect.equal "Mercury"
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
                        Birthdays.calculate_birthdays birthdate today

                    day =
                        Maybe.withDefault bad_planet
                            (List.head (List.filter (\m -> m.planet_name == "Earth") days))
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
                        Birthdays.calculate_birthdays birthdate today

                    day =
                        Maybe.withDefault
                            bad_planet
                            (List.head (List.filter (\m -> m.planet_name == "Earth") days))
                in
                day.earth_date |> Expect.equal (Date.fromCalendarDate 2022 Sep 16)
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
                    planetary_birthdays =
                        calculate_birthdays
                            (Date.fromCalendarDate 1965 Feb 13)
                            (Date.fromCalendarDate 2022 May 6)

                    jupiterish =
                        List.head (List.drop 4 planetary_birthdays)
                in
                case jupiterish of
                    Just jupiter ->
                        Date.compare jupiter.earth_date (Date.fromCalendarDate 2024 Jun 5) |> Expect.equal EQ

                    _ ->
                        Debug.todo "Never can reach here"
        ]
