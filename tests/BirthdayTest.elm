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


isMercury : PlanetaryBirthday -> Bool
isMercury planetaryBirthday =
    planetaryBirthday.planetName == "Mercury"


isVenus : PlanetaryBirthday -> Bool
isVenus planetaryBirthday =
    planetaryBirthday.planetName == "Venus"


isEarth : PlanetaryBirthday -> Bool
isEarth planetaryBirthday =
    planetaryBirthday.planetName == "Earth"


isJupiter : PlanetaryBirthday -> Bool
isJupiter planetaryBirthday =
    planetaryBirthday.planetName == "Jupiter"


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
                            (List.head (List.filter isEarth days))
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
                            (List.head (List.filter isEarth days))
                in
                day.earthDate
                    |> Expect.equal (Date.fromCalendarDate 2023 Jan 31)
        , test "check Jupiter birthday occurs (11.8618 * 365.25) days later" <|
            \_ ->
                let
                    planetaryBirthdays =
                        calculateBirthdays
                            (Date.fromCalendarDate 2022 May 15)
                            (Date.fromCalendarDate 2022 Aug 10)

                    jupiterish =
                        List.head (List.filter isJupiter planetaryBirthdays)
                in
                case jupiterish of
                    Just jupiter ->
                        Date.compare jupiter.earthDate
                            (Date.fromCalendarDate 2034 Mar 25)
                            |> Expect.equal EQ

                    _ ->
                        Debug.todo "Never can reach here"
        , test "check Mercury birthday occurs 87.969 days later" <|
            \_ ->
                let
                    planetaryBirthdays =
                        calculateBirthdays
                            (Date.fromCalendarDate 2022 May 15)
                            (Date.fromCalendarDate 2022 Aug 10)

                    oneOnMercury =
                        List.head (List.filter isMercury planetaryBirthdays)
                in
                case oneOnMercury of
                    Just mercury ->
                        let
                            _ =
                                Debug.log "mercury.earthDay" (Date.day mercury.earthDate)

                            _ =
                                Debug.log "mercury.earthMon" (Date.month mercury.earthDate)

                            _ =
                                Debug.log "mercury.earthYr" (Date.year mercury.earthDate)
                        in
                        Date.compare mercury.earthDate
                            (Date.fromCalendarDate 2022 Aug 10)
                            |> Expect.equal EQ

                    _ ->
                        Debug.todo "Never can reach here"
        , test "check Venus birthday occurs 224.701 days later" <|
            \_ ->
                let
                    planetaryBirthdays =
                        calculateBirthdays
                            (Date.fromCalendarDate 2022 May 15)
                            (Date.fromCalendarDate 2022 Aug 10)

                    oneOnVenus =
                        List.head (List.filter isVenus planetaryBirthdays)
                in
                case oneOnVenus of
                    Just venus ->
                        Date.compare venus.earthDate
                            (Date.fromCalendarDate 2022 Dec 25)
                            |> Expect.equal EQ

                    _ ->
                        Debug.todo "Never can reach here"
        ]
