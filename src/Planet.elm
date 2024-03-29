module Planet exposing (OrbitDays, Planet(..), planets, toName)

{-| Which planets and what we know what about them
-}


type alias OrbitDays =
    Float


type alias Name =
    String


type Planet
    = Earth Name
    | Alien Name OrbitDays



{- For planets that take longer than an Earth year to orbit Sun
   Can't use this value on Earth, because a calendar defines
   a year on Earth; not a fixed-measure of time
-}


earthYearInDays : Float
earthYearInDays =
    365.2564363004


planets : List Planet
planets =
    [ -- order important for list manipulation by BirthdayTest.elm
      Alien "Mercury" 87.969
    , Alien "Venus" 224.701
    , Earth "Earth"
    , Alien "Mars" 686.98
    , Alien "Jupiter" (11.8618 * earthYearInDays)
    , Alien "Saturn" (29.4571 * earthYearInDays)
    , Alien "Uranus" (84.0205 * earthYearInDays)
    , Alien "Neptune" (164.81 * earthYearInDays)
    , Alien "Pluto" (247.94 * earthYearInDays)
    ]


toName : Planet -> Name
toName planet =
    case planet of
        Earth name ->
            name

        Alien name _ ->
            name
