module Planet exposing (OrbitDays, Planet(..), planets, toName)

type alias OrbitDays =
    Float

type alias Name =
    String

type Planet
    = Earth Name
    | Alien Name OrbitDays


earth_year_in_days : Float
earth_year_in_days =
    365.2564363004

planets : List Planet
planets = 
    [
        -- order important for list manipulation by BirthdayTest.elm
        ( Alien "Mercury" 87.969 )
        , ( Alien "Venus" 224.701 )
        , ( Earth "Earth" )
        , ( Alien "Mars" 686.980 )
        , ( Alien "Jupiter" ( 11.8618 * earth_year_in_days ) )
        , ( Alien "Saturn" ( 29.4571 * earth_year_in_days ) )
        , ( Alien "Uranus" ( 84.0205 * earth_year_in_days ) )
        , ( Alien "Neptune" ( 164.81 * earth_year_in_days ) )
        , ( Alien "Pluto" ( 247.94 * earth_year_in_days)  )
    ]

toName : Planet -> Name
toName planet =
    case planet of
        Earth name ->
            name

        Alien name _ ->
            name

