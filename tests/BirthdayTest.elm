module BirthdayTest exposing (..)

import Birthdays
import Css exposing (Ex)
import Date
import Expect
import Main
import Test exposing (Test, describe, test)
import Time exposing (Month(..))


fallbackDate : Date.Date
fallbackDate =
    Date.fromCalendarDate 1 Jan 1


badPlanet : Birthdays.PlanetaryBirthday
badPlanet =
    { planetName = "BadPlanet"
    , age = 99999
    , date = fallbackDate
    , todayOnEarth = fallbackDate
    }


notMercury : Birthdays.PlanetaryBirthday
notMercury =
    { planetName = "NotMercury"
    , age = 21
    , date = fallbackDate
    , todayOnEarth = fallbackDate
    }


isMercury : Birthdays.PlanetaryBirthday -> Bool
isMercury planetaryBirthday =
    planetaryBirthday.planetName == "Mercury"


isVenus : Birthdays.PlanetaryBirthday -> Bool
isVenus planetaryBirthday =
    planetaryBirthday.planetName == "Venus"


isEarth : Birthdays.PlanetaryBirthday -> Bool
isEarth planetaryBirthday =
    planetaryBirthday.planetName == "Earth"


isJupiter : Birthdays.PlanetaryBirthday -> Bool
isJupiter planetaryBirthday =
    planetaryBirthday.planetName == "Jupiter"


simple : Test
simple =
    describe "simple utility functions"
        [ test "9 birthdays returned" returnsNineBirthdays
        , test "Mercury is first planet of birthdays?" mercuryFirstPlanet
        ]


returnsNineBirthdays : () -> Expect.Expectation
returnsNineBirthdays () =
    let
        birthdate =
            Date.fromCalendarDate 2000 Feb 28

        today =
            Date.fromCalendarDate 2022 May 1
    in
    (List.length <| Birthdays.calculateBirthdays birthdate today)
        |> Expect.equal 9


mercuryFirstPlanet : () -> Expect.Expectation
mercuryFirstPlanet () =
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


forEarthTestAge : () -> Expect.Expectation
forEarthTestAge () =
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


forEarthBirthdayTestSameDay : () -> Expect.Expectation
forEarthBirthdayTestSameDay () =
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
    day.date
        |> Expect.equal (Date.fromCalendarDate 2023 Jan 31)


forJupiterTestBirthday : () -> Expect.Expectation
forJupiterTestBirthday () =
    let
        planetaryBirthdays =
            Birthdays.calculateBirthdays
                (Date.fromCalendarDate 2022 May 15)
                (Date.fromCalendarDate 2022 Aug 10)

        jupiterish =
            List.head (List.filter isJupiter planetaryBirthdays)
    in
    case jupiterish of
        Just jupiter ->
            Date.compare jupiter.date
                (Date.fromCalendarDate 2034 Mar 25)
                |> Expect.equal EQ

        _ ->
            Debug.todo "Never can reach here"


forMercuryTestBirthday : () -> Expect.Expectation
forMercuryTestBirthday () =
    let
        planetaryBirthdays =
            Birthdays.calculateBirthdays
                (Date.fromCalendarDate 2022 May 15)
                (Date.fromCalendarDate 2022 Aug 10)

        oneOnMercury =
            List.head (List.filter isMercury planetaryBirthdays)
    in
    case oneOnMercury of
        Just mercury ->
            Date.compare mercury.date
                (Date.fromCalendarDate 2022 Aug 10)
                |> Expect.equal EQ

        _ ->
            Debug.todo "Never can reach here"


forVenusTestBirthday : () -> Expect.Expectation
forVenusTestBirthday () =
    let
        planetaryBirthdays =
            Birthdays.calculateBirthdays
                (Date.fromCalendarDate 2022 May 15)
                (Date.fromCalendarDate 2022 Aug 10)

        oneOnVenus =
            List.head (List.filter isVenus planetaryBirthdays)
    in
    case oneOnVenus of
        Just venus ->
            Date.compare venus.date
                (Date.fromCalendarDate 2022 Dec 25)
                |> Expect.equal EQ

        _ ->
            Debug.todo "Never can reach here"


forFutureTestAge1Not0 : () -> Expect.Expectation
forFutureTestAge1Not0 () =
    let
        planetaryBirthdays =
            Birthdays.calculateBirthdays
                (Date.fromCalendarDate 2022 May 15)
                (Date.fromCalendarDate 2022 May 14)

        oneOnVenus =
            List.head (List.filter isVenus planetaryBirthdays)
    in
    case oneOnVenus of
        Just venus ->
            venus.age
                == 1
                |> Expect.equal True

        _ ->
            Debug.todo "Never can reach here"


forAlienBirthdateTodayTestAgeIs1 : () -> Expect.Expectation
forAlienBirthdateTodayTestAgeIs1 () =
    let
        planetaryBirthdays =
            Birthdays.calculateBirthdays
                (Date.fromCalendarDate 2022 May 15)
                (Date.fromCalendarDate 2022 May 15)

        oneOnVenus =
            List.head (List.filter isVenus planetaryBirthdays)
    in
    case oneOnVenus of
        Just venus ->
            venus.age
                == 1
                |> Expect.equal True

        _ ->
            Debug.todo "Never can reach here"


forCurrentYearTestNoFutureMonthsOffered : () -> Expect.Expectation
forCurrentYearTestNoFutureMonthsOffered () =
    let
        today =
            Date.fromCalendarDate 2022 Mar 1

        months =
            Main.monthNames 2022 today
    in
    List.length months
        |> Expect.equal 3


isTodayABirthday : () -> Expect.Expectation
isTodayABirthday () =
    let
        today =
            Date.fromCalendarDate 2022 Mar 1

        birthday =
            Date.fromCalendarDate 2000 Mar 1
    in
    Birthdays.isBirthdayToday birthday today |> Expect.equal True


isAgeCorrectWhenTodayIsBirthday : () -> Expect.Expectation
isAgeCorrectWhenTodayIsBirthday () =
    Birthdays.nextEarthAge (Date.fromCalendarDate 2021 Sep 1) (Date.fromCalendarDate 2022 Sep 1) |> Expect.equal 1


complex : Test
complex =
    describe "date functions"
        [ test "Earth birthday is correct age?" forEarthTestAge
        , test "Earth birthday is same day of month/year?" forEarthBirthdayTestSameDay
        , test "check Jupiter birthday occurs (11.8618 * 365.25) days later" forJupiterTestBirthday
        , test "check Mercury birthday occurs 87.969 days later" forMercuryTestBirthday
        , test "check Venus birthday occurs 224.701 days later" forVenusTestBirthday
        , test "check future birthdate calculates age as 1 not 0" forFutureTestAge1Not0
        , test "check birthdate falling today calculates age as 1" forAlienBirthdateTodayTestAgeIs1
        , test "check no future months in current year offered" forCurrentYearTestNoFutureMonthsOffered
        , test "is today a birthday?"
            isTodayABirthday
        , test "check birthday falling today age today"
            isAgeCorrectWhenTodayIsBirthday
        ]
