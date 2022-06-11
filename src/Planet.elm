module Planet exposing (OrbitDays, Planet(..), planets, toName)

type alias OrbitDays =
    Float

type alias Name =
    String

type Planet
    = Earth Name
    | Alien Name OrbitDays

planets : List Planet
planets = 
    [
        -- order important for list manipulation by BirthdayTest.elm
        ( Alien "Mercury" 88.0 )
        , ( Alien "Venus" 224.68 )
        , ( Earth "Earth" )
        , ( Alien "Mars" 686.98 )
        , ( Alien "Jupiter" ( 11.862 * 365.25 ) )
        , ( Alien "Saturn" ( 29.456 * 365.25 ) )
        , ( Alien "Uranus" ( 84.08 * 365.25 ) )
        , ( Alien "Neptune" ( 164.81 * 365.25 ) )
        , ( Alien "Pluto" ( 247.7 * 365.25)  )
    ]

toName : Planet -> Name
toName planet =
    case planet of
        Earth name ->
            name

        Alien name _ ->
            name

