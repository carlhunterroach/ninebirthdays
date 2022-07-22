module App exposing (Model, Msg, app)

import Birthdays exposing (Birthdate, PlanetaryBirthday, Today)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Date exposing (Date)
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Html exposing (Attribute, Html, a, button, div, h1, h2, img, li, option, p, select, span, table, td, text, tr, ul)
import Html.Attributes exposing (alt, href, src, style, title, value)
import Html.Events exposing (onClick, onInput)
import Ordinal
import Task
import Time exposing (Month(..))
import Tuple
import Url


type alias Year =
    Int


type Msg
    = DayPicked String
    | MonthPicked String
    | YearPicked String
    | AppStarted Today
    | LinkedClicked Browser.UrlRequest
    | TryingElm
    | UrlChanged Url.Url
    | ChangeUrl Url.Url



-- MAIN ENTRY POINT


app : Program () Model Msg
app =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkedClicked
        }



-- MODEL


type alias Model =
    { userBirthdate : Birthdate
    , birthdays : List PlanetaryBirthday
    , today : Today
    , key : Nav.Key
    , url : Url.Url
    }



-- FUNCTIONS


initModel : Url.Url -> Nav.Key -> Model
initModel url key =
    { userBirthdate = fallbackBirthdate
    , birthdays = dummyBirthdaysReplacedByAppStarted
    , today = dummyTodayReplacedByAppStarted
    , key = key
    , url = url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( initModel url key
    , Task.perform AppStarted Date.today
    )


isLeapYear : Year -> Bool
isLeapYear y =
    modBy 4 y == 0 && modBy 100 y /= 0 || modBy 400 y == 0


fallbackDay : number
fallbackDay =
    1


fallbackMonth : Month
fallbackMonth =
    Jan


defaultDayOr : String -> Int
defaultDayOr dayString =
    Maybe.withDefault fallbackDay (String.toInt dayString)


defaultMonthOr : String -> Month
defaultMonthOr monthString =
    Date.numberToMonth (Maybe.withDefault fallbackMonthValue (String.toInt monthString))


defaultYearOr : String -> Int
defaultYearOr yearString =
    Maybe.withDefault fallbackYear (String.toInt yearString)


fallbackMonthValue : number
fallbackMonthValue =
    1


fallbackYear : number
fallbackYear =
    2000


fallbackBirthdate : Birthdate
fallbackBirthdate =
    Date.fromCalendarDate fallbackYear fallbackMonth fallbackDay


dummyTodayReplacedByAppStarted : Date
dummyTodayReplacedByAppStarted =
    Date.fromCalendarDate 2022 May 1


dummyBirthdaysReplacedByAppStarted : List PlanetaryBirthday
dummyBirthdaysReplacedByAppStarted =
    []



-- set Day input to correct number of days for Month/Year picked


updateModelWithBirthdate : Model -> Birthdate -> Model
updateModelWithBirthdate model newBirthdate =
    { model
        | userBirthdate = newBirthdate
        , birthdays = Birthdays.calculateBirthdays newBirthdate model.today
    }


updateBirthdateDay : Birthdate -> String -> Birthdate
updateBirthdateDay birthdate dayString =
    Date.fromCalendarDate
        (Date.year birthdate)
        (Date.month birthdate)
        (defaultDayOr dayString)


updateBirthdateMonth : Birthdate -> String -> Birthdate
updateBirthdateMonth birthdate monthString =
    Date.fromCalendarDate
        (Date.year birthdate)
        (defaultMonthOr monthString)
        (Date.day birthdate)


updateBirthdateYear : Birthdate -> String -> Birthdate
updateBirthdateYear birthdate yearString =
    Date.fromCalendarDate
        (defaultYearOr yearString)
        (Date.month birthdate)
        (Date.day birthdate)


onBirthdateChanged : Model -> Birthdate -> ( Model, Cmd Msg )
onBirthdateChanged model birthdate =
    ( updateModelWithBirthdate model birthdate
    , Nav.pushUrl model.key ("?" ++ Date.toIsoString birthdate)
    )


onBirthdatePicked : Model -> Birthdate -> ( Model, Cmd Msg )
onBirthdatePicked model birthdate =
    onBirthdateChanged model birthdate


onAppStarted : Model -> Today -> ( Model, Cmd Msg )
onAppStarted model today =
    let
        newBirthdate =
            birthdateFromUrl model.url
    in
    ( { model
        | userBirthdate = newBirthdate
        , today = today
        , birthdays = Birthdays.calculateBirthdays newBirthdate today
      }
    , Cmd.none
    )


onChangeBirthdateUrl : Model -> Url.Url -> ( Model, Cmd Msg )
onChangeBirthdateUrl model url =
    let
        newBirthdate =
            birthdateFromUrl url
    in
    ( { model
        | userBirthdate = newBirthdate
        , birthdays = Birthdays.calculateBirthdays newBirthdate model.today
      }
    , newTopPage url
    )


newTopPage : Url.Url -> Cmd Msg
newTopPage url =
    Task.perform (\_ -> ChangeUrl url) (Dom.setViewport 0 0)


onLinkedClicked : Model -> Browser.UrlRequest -> ( Model, Cmd Msg )
onLinkedClicked model urlRequest =
    case urlRequest of
        Browser.Internal url ->
            onChangeBirthdateUrl model url

        Browser.External href ->
            ( model
            , Nav.load href
            )


onUrlChanged : Model -> Url.Url -> ( Model, Cmd Msg )
onUrlChanged model url =
    ( { model
        | userBirthdate = birthdateFromUrl url
        , url = url
      }
    , Cmd.none
    )


onChangeUrl : Model -> Url.Url -> ( Model, Cmd Msg )
onChangeUrl model url =
    ( model
    , Nav.pushUrl model.key (Url.toString url)
    )


onTryingElm : Model -> ( Model, Cmd Msg )
onTryingElm model =
    ( model
    , Nav.load "/trying-elm.html"
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DayPicked dayString ->
            onBirthdatePicked model (updateBirthdateDay model.userBirthdate dayString)

        MonthPicked monthString ->
            onBirthdatePicked model (updateBirthdateMonth model.userBirthdate monthString)

        YearPicked yearString ->
            onBirthdatePicked model (updateBirthdateYear model.userBirthdate yearString)

        AppStarted newToday ->
            onAppStarted model newToday

        LinkedClicked urlRequest ->
            onLinkedClicked model urlRequest

        UrlChanged url ->
            onUrlChanged model url

        ChangeUrl url ->
            onChangeUrl model url

        TryingElm ->
            onTryingElm model


isCommonEra : Date -> Bool
isCommonEra date =
    Date.year date > 0


birthdateFromUrl : Url.Url -> Birthdate
birthdateFromUrl url =
    case Maybe.map Date.fromIsoString <| url.query of
        Just (Ok birthdate) ->
            if isCommonEra birthdate then
                birthdate

            else
                fallbackBirthdate

        _ ->
            fallbackBirthdate


smallSpacer : Html msg
smallSpacer =
    span [ style "font-size" "40%" ] [ text " " ]


type WhenIsBirthday
    = Today
    | Future


isBirthdayToday : PlanetaryBirthday -> WhenIsBirthday
isBirthdayToday birthday =
    if
        Date.day birthday.earthDate
            == Date.day birthday.todayOnEarth
            && Date.month birthday.earthDate
            == Date.month birthday.todayOnEarth
    then
        Today

    else
        Future


ordinalisedDate : Date -> String
ordinalisedDate date =
    Ordinal.ordinal (Date.format "d" date) ++ Date.format " MMM y" date


smartBirthdayMessage : PlanetaryBirthday -> List (Html msg)
smartBirthdayMessage birthday =
    {-
       specical case: if today is a birthday, then
       while technically the next birthday is an orit away
       we really should shout out Happy Birthday today!
    -}
    case isBirthdayToday birthday of
        Today ->
            [ span
                [ style "font-family" "cursive" ]
                [ text "Happy Birthday today!" ]
            ]

        Future ->
            [ text (ordinalisedDate birthday.earthDate)
            ]


smartAge : PlanetaryBirthday -> Int
smartAge birthday =
    {-
       special case: if today is a birthday, then
       don't just return the up and coming age
       but the current age
    -}
    case isBirthdayToday birthday of
        Today ->
            birthday.age - 1

        Future ->
            birthday.age


smartRowStyle : PlanetaryBirthday -> List (Attribute msg)
smartRowStyle birthday =
    case isBirthdayToday birthday of
        Today ->
            [ style "color" "yellow"
            , style "font-family" "cursive"
            , style "font-size" "105%"
            ]

        Future ->
            [ style "" "" ]


viewBirthday : PlanetaryBirthday -> Html Msg
viewBirthday birthday =
    let
        ageSmart =
            smartAge birthday

        suffix =
            if ageSmart == 1 then
                " yr"

            else
                " yrs"

        formattedAge =
            FormatNumber.format
                { usLocale
                    | decimals = Exact 0
                }
                (toFloat ageSmart)
    in
    tr (smartRowStyle birthday)
        [ td
            [ style "text-align" "left"
            , style "padding-left" "0.5em"
            ]
            [ text birthday.planetName ]
        , td
            [ style "text-align" "center"
            , style "padding-right" "0.1em"
            ]
            (smartBirthdayMessage birthday)
        , td
            [ style "padding-left" "0.5em"
            , style "padding-right" "0.5em"
            ]
            [ span [] [ text formattedAge ]
            , smallSpacer
            , span [ style "font-size" "80%" ] [ text suffix ]
            , smallSpacer
            , span [ style "font-size" "80%" ] [ text "old" ]
            ]
        ]



-- VIEW


birthdayTableHeading : List (Attribute msg)
birthdayTableHeading =
    [ style "text-align" "center"
    , style "background-color" "rgba(50,50,50,.7)"
    , style "border-radius" "15px"
    , style "margin-left" "auto"
    , style "margin-right" "auto"
    , style "margin-top" "1.8em"
    ]


footer : List (Attribute msg)
footer =
    [ style "font-size" "60%"
    , style "font-family" "Arial, sans-serif"
    , style "color" "black"
    , style "margin" "5em 5em 5em 5em"
    ]


monthOptions : ( Int, String ) -> Html Msg
monthOptions ( monthZeroIndexed, string ) =
    option [ value (String.fromInt (monthZeroIndexed + 1)) ] [ text string ]


dayOption : Int -> Html msg
dayOption day =
    option [ value (String.fromInt day) ] [ text (String.fromInt day) ]


dayOptions : Birthdate -> List (Html msg)
dayOptions birthdate =
    let
        dayMax =
            case Date.month birthdate of
                Feb ->
                    if isLeapYear (Date.year birthdate) then
                        29

                    else
                        28

                other ->
                    if List.member other [ Apr, Jun, Sep, Nov ] then
                        30

                    else
                        31
    in
    List.map dayOption (List.range 1 dayMax)


yearOptions : Int -> Html msg
yearOptions year =
    option [ value (String.fromInt year) ] [ text (String.fromInt year) ]


namesOfMonth : List String
namesOfMonth =
    [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" ]


dateInputs : Model -> Html Msg
dateInputs model =
    div []
        [ select
            [ style "font-size" "1.5em"
            , style "border-radius" ".2em"
            , onInput DayPicked
            , value (String.fromInt (Date.day model.userBirthdate))
            ]
            (dayOptions model.userBirthdate)
        , select
            [ style "font-size" "1.5em"
            , style "margin-left" ".3em"
            , style "margin-right" ".3em"
            , style "border-radius" ".2em"
            , onInput MonthPicked
            , value (String.fromInt (Date.monthNumber model.userBirthdate))
            ]
            (List.map monthOptions (List.indexedMap Tuple.pair namesOfMonth))
        , select
            [ style "font-size" "1.5em"
            , style "border-radius" ".2em"
            , onInput YearPicked
            , value (String.fromInt (Date.year model.userBirthdate))
            ]
            (List.map yearOptions (List.range 1 (Date.year model.today)))
        ]


vipLinks : List ( String, String )
vipLinks =
    [ ( "1879-03-14", "Albert Einstein" )
    , ( "1452-04-15", "Leonardo da Vinci" )
    , ( "1856-07-28", "Nikola Tesla" )
    , ( "1955-06-08", "Tim Berners-Lee" )
    , ( "1847-02-11", "Thomas Edison" )
    , ( "1847-03-03", "Alexander Graham Bell" )
    , ( "1947-05-02", "James Dyson" )
    , ( "1468-02-03", "Johannes Gutenberg" )
    , ( "1940-12-01", "Jerry Lawson" )
    , ( "1928-04-06", "James Watson" )
    , ( "1916-06-08", "Francis Crick" )
    , ( "1881-10-25", "Pablo Picasso" )
    , ( "1475-03-06", "“Michelangelo”" )
    , ( "1856-01-12", "John Singer Sargent" )
    , ( "1916-12-15", "Maurice Wilkins" )
    , ( "1949-10-06", "Lonnie Johnson" )
    , ( "1922-10-30", "Marie Van Brittan Brown" )
    , ( "1862-10-19", "Auguste Lumière" )
    , ( "1864-10-05", "Louis Lumière" )
    , ( "1946-08-05", "Dr. Shirley Jackson" )
    , ( "1867-11-07", "Marie Curie" )
    , ( "1794-12-28", "Nancy Johnson" )
    , ( "1900-12-12", "Maria Telkes" )
    , ( "1952-07-06", "Ann Tsukamoto" )
    , ( "1906-12-09", "Grace Hopper" )
    , ( "1866-05-09", "Elizabeth “Lizzie” Magie" )
    , ( "1920-07-25", "Rosalind Franklin" )
    , ( "1923-07-31", "Stephanie Kwolek" )
    ]


compareVIPs : ( String, String ) -> ( String, String ) -> Order
compareVIPs a b =
    if Tuple.second a < Tuple.second b then
        LT

    else
        GT


viewVIPLink : ( String, String ) -> Html Msg
viewVIPLink ( path, name ) =
    li [ style "list-style-type" "none" ]
        [ a
            [ href ("?" ++ path)
            , style "text-decoration" "none"
            ]
            [ text name ]
        ]


celebrationTexts : List String
celebrationTexts =
    [ "On Mercury, spend sometime taking in the countryside"
    , "For a Venusian birthday, make a new favourite meal - no cardboard containers, please"
    , "On homely Earth, there is so much to celebrate and so much to do - try doing it with cake!!"
    , "Grasp your Mars anniversary and go see a play or movie"
    , "Jupiter birthdays are precious and rare - do something for the first time!"
    , "Saturn starts a weekend whenever it falls so make time to celebrate with a friend"
    , "Find one of the great inventors from history and mark their Uranus birthday with panache!"
    , "Celebrate a great artist's Neptune birthday and do it wearing a hat :)"
    , "Make your mark, here on planet Earth, and have the globe celebrate your first Plutonian birthday!!"
    ]


viewCelebration : String -> Html Msg
viewCelebration suggestion =
    li [ style "list-style-type" "none" ] [ text suggestion ]


view : Model -> Browser.Document Msg
view model =
    { title = "9Birthdays"
    , body =
        [ div
            [ style "background-image" "url(/solar-system.png)"
            , style "background-repeat" "no-repeat"
            , style "width" "1000px"
            , style "color" "white"
            , style "text-align" "center"
            , style "background-color" "dark-gray"
            , style "margin" ".5em .5em .5em .5em"
            , style "font-family" "Arial, sans-serif"
            , style "font-size" "1.5em"
            ]
            [ h1
                [ style "font-size" "3em"
                ]
                [ img
                    [ style "border-radius" "15px"
                    , style "background-color" "black"
                    , style "padding" ".3em .3em .3em .3em"
                    , style "margin-top" "-1.1em"
                    , src "/logo.png"
                    , alt "Find your 9Birthdays"
                    , title "Find your 9Birthdays"
                    ]
                    []
                ]
            , h2 [ style "margin-top" "-1.8em" ] [ text "Born on this day?" ]
            , dateInputs model
            , h2
                [ style "margin-bottom" "0"
                , style "margin-top" "1em"
                ]
                [ text "Your next planetary birthdays are:" ]
            , table birthdayTableHeading
                (List.map viewBirthday <| List.sortWith Birthdays.compare model.birthdays)
            , div
                [ style "color" "black"
                , style "padding-top" "3em"
                ]
                [ div
                    [ style "margin-top" "10em"
                    , style "padding-bottom" "0"
                    , style "margin-bottom" "0"
                    ]
                    [ p [] [ text "Did you know we have birthdays on each planet in our solar system?" ]
                    , p
                        [ style "font-size" "70%"
                        , style "margin-top" "-1em"
                        ]
                        [ text "It's true, enter your birthdate above and we'll calcuate your 9 birthdays" ]
                    ]
                , div [ style "padding-top" "0.2em" ]
                    [ span [] [ text "A few examples" ]
                    , ul [ style "font-size" "75%" ]
                        (List.map viewVIPLink <| List.sortWith compareVIPs vipLinks)
                    ]
                , div []
                    [ p [] [ text "How to celebrate your 9 planetary birthdays" ]
                    , ul [ style "font-size" "60%" ]
                        (List.map viewCelebration celebrationTexts)
                    ]
                ]
            , div footer
                [ div [ style "font-family" "Arial, sans-serif" ]
                    [ text "A small elm project, hosted by InfinityFree" ]

                {-
                   use button to "hyperlink" out of app
                   true [a] tags reserved for LinkedClicked msgs
                   this avoids parsing types of internal URLs
                -}
                , div
                    [ style "font-family" "Arial, sans-serif"
                    , style "margin-top" "0.2em"
                    ]
                    [ button
                        [ onClick TryingElm
                        , style "text-decoration" "underline"
                        , style "cursor" "pointer"
                        , style "border" "none"
                        , style "color" "#3894FF"
                        , style "background-color" "white"
                        , style "padding" "0.6em"
                        , style "border-radius" "5px"
                        ]
                        [ text "My experiences trying Elm" ]
                    ]
                ]
            ]
        ]
    }
