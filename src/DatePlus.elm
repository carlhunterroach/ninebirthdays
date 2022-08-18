module DatePlus exposing (daysSinceDate, isLeapYear, yearsSinceDate)

import Date exposing (Date)


{-|

    isLeapYear 2001
    --> False

    isLeapYear 2000
    --> True

    isLeapYear 1900
    --> False

    isLeapYear 1984
    --> True

-}
isLeapYear : Int -> Bool
isLeapYear y =
    modBy 4 y == 0 && modBy 100 y /= 0 || modBy 400 y == 0


{-|

    import Date exposing(Date)
    import Time exposing(Month(..))

    -- is next day one day away?
    daysSinceDate
        (Date.fromCalendarDate 2000 Jan 1)
        (Date.fromCalendarDate 2000 Jan 2)
    --> 1

    -- is next year, across leap year, a full year away?
    daysSinceDate
        (Date.fromCalendarDate 2000 Jan 1)
        (Date.fromCalendarDate 2001 Jan 1)
    --> 366

    -- 1 leap year + 1 non-leap year?
    daysSinceDate
        (Date.fromCalendarDate 2000 Jan 1)
        (Date.fromCalendarDate 2002 Jan 1)
    --> 366 + 365

-}
daysSinceDate : Date -> Date -> Int
daysSinceDate fromDate toDate =
    -- don't enable future date like fromDates > toDate
    Basics.max 0 (Date.diff Date.Days fromDate toDate)


{-|

    import Date exposing(Date)
    import Time exposing(Month(..))

    yearsSinceDate
        (Date.fromCalendarDate 2000 Jan 1)
        (Date.fromCalendarDate 2002 Jan 1)
    --> 2

    yearsSinceDate
        (Date.fromCalendarDate 2000 Jan 1)
        (Date.fromCalendarDate 2000 Dec 31)
    --> 0

-}
yearsSinceDate : Date -> Date -> Int
yearsSinceDate fromDate toDate =
    -- don't enable future date like fromDates > toDate
    Basics.max 0 (Date.diff Date.Years fromDate toDate)
