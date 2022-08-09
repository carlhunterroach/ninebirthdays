module Main exposing (main)

{-| A mind-alternating bit of silliness called 9 Birthdays

    The premise goes...
        you have a birthday each time a planet in our solar system
        completes an orbit around our sun.
        The starting points in time of each of their orbits
        is the day you were born: your birthdate.

    Enter your birthdate into 9 Birthdays and it'll tell you
    when your next birthday, on each planet, is due to fall.

-}

import App exposing (Model, Msg, app)


main : Program () Model Msg
main =
    app
