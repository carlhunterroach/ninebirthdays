module App exposing (Model, Msg, app)

import Birthdays exposing (Birthdate, PlanetaryBirthday, Today)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Date exposing (Date)
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Html exposing (Attribute, Html, a, div, h1, h2, img, li, option, p, select, span, table, td, text, tr, ul)
import Html.Attributes exposing (alt, href, src, style, title, value)
import Html.Events exposing (onInput)
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
    { user_birthdate : Birthdate
    , birthdays : List PlanetaryBirthday
    , today : Today
    , key : Nav.Key
    , url : Url.Url
    }

-- FUNCTIONS

initModel : Url.Url -> Nav.Key -> Model
initModel url key =
    { user_birthdate = fallbackBirthdate
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


is_leap_year : Year -> Bool
is_leap_year y =
    modBy 4 y == 0 && modBy 100 y /= 0 || modBy 400 y == 0


fallbackDay : number
fallbackDay =
    1


fallbackMonth : Month
fallbackMonth =
    Jan


default_day_or : String -> Int
default_day_or dayString =
    Maybe.withDefault fallbackDay (String.toInt dayString)


default_month_or : String -> Month
default_month_or monthString =
    Date.numberToMonth (Maybe.withDefault fallbackMonthValue (String.toInt monthString))


default_year_or : String -> Int
default_year_or yearString =
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


update_model_with_birthdate : Model -> Birthdate -> Model
update_model_with_birthdate model new_birthdate =
    { model
        | user_birthdate = new_birthdate
        , birthdays = Birthdays.calculate_birthdays new_birthdate model.today
    }


update_birthdate_day : Birthdate -> String -> Birthdate
update_birthdate_day birthdate dayString =
    Date.fromCalendarDate
        (Date.year birthdate)
        (Date.month birthdate)
        (default_day_or dayString)


update_birthdate_month : Birthdate -> String -> Birthdate
update_birthdate_month birthdate monthString =
    Date.fromCalendarDate
        (Date.year birthdate)
        (default_month_or monthString)
        (Date.day birthdate)


update_birthdate_year : Birthdate -> String -> Birthdate
update_birthdate_year birthdate yearString =
    Date.fromCalendarDate
        (default_year_or yearString)
        (Date.month birthdate)
        (Date.day birthdate)


on_birthdate_changed : Model -> Birthdate -> ( Model, Cmd Msg )
on_birthdate_changed model birthdate =
    ( update_model_with_birthdate model birthdate
    , Nav.pushUrl model.key ("?" ++ Date.toIsoString birthdate)
    )


on_birthdate_picked : Model -> Birthdate -> ( Model, Cmd Msg )
on_birthdate_picked model birthdate =
    on_birthdate_changed model birthdate


on_app_started : Model -> Today -> ( Model, Cmd Msg )
on_app_started model today =
    let
        new_birthdate =
            birthdate_from_url model.url
    in
    ( { model
        | user_birthdate = new_birthdate
        , today = today
        , birthdays = Birthdays.calculate_birthdays new_birthdate today
      }
    , Cmd.none
    )


on_change_birthdate_url : Model -> Url.Url -> ( Model, Cmd Msg )
on_change_birthdate_url model url =
    let
        new_birthdate =
            birthdate_from_url url
    in
    ( { model
        | user_birthdate = new_birthdate
        , birthdays = Birthdays.calculate_birthdays new_birthdate model.today
      }
    , new_top_page url
    )


new_top_page : Url.Url -> Cmd Msg
new_top_page url =
    Task.perform (\_ -> ChangeUrl url) (Dom.setViewport 0 0)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DayPicked dayString ->
            on_birthdate_picked model (update_birthdate_day model.user_birthdate dayString)

        MonthPicked monthString ->
            on_birthdate_picked model (update_birthdate_month model.user_birthdate monthString)

        YearPicked yearString ->
            on_birthdate_picked model (update_birthdate_year model.user_birthdate yearString)

        AppStarted new_today ->
            on_app_started model new_today

        LinkedClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    on_change_birthdate_url model url

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            ( { model
                | user_birthdate = birthdate_from_url url
                , url = url
              }
            , Cmd.none
            )

        ChangeUrl url ->
            ( model
            , Nav.pushUrl model.key (Url.toString url)
            )


birthdate_from_url : Url.Url -> Birthdate
birthdate_from_url url =
    case Maybe.map Date.fromIsoString <| url.query of
        Just (Ok birthdate) ->
            birthdate

        _ ->
            fallbackBirthdate


smallSpacer : Html msg
smallSpacer =
    span [ style "font-size" "40%" ] [ text " " ]


type WhenIsBirthday
    = Today
    | Future


is_birthday_today : PlanetaryBirthday -> WhenIsBirthday
is_birthday_today birthday =
    if
        Date.day birthday.earth_date
            == Date.day birthday.today_on_earth
            && Date.month birthday.earth_date
            == Date.month birthday.today_on_earth
    then
        Today

    else
        Future


ordinalised_date : Date -> String
ordinalised_date date =
    let
        day =
            Date.format "d" date
    in
    Ordinal.ordinal day ++ Date.format " MMM y" date


smart_birthday_message : PlanetaryBirthday -> List (Html msg)
smart_birthday_message birthday =
    {-
       specical case: if today is a birthday, then
       while technically the next birthday is an orit away
       we really should shout out Happy Birthday today!
    -}
    case is_birthday_today birthday of
        Today ->
            [ span
                [ style "font-family" "cursive" ]
                [ text "Happy Birthday today!" ]
            ]

        Future ->
            [ text (ordinalised_date birthday.earth_date)
            ]


smart_age : PlanetaryBirthday -> Int
smart_age birthday =
    {-
       special case: if today is a birthday, then
       don't just return the up and coming age
       but the current age
    -}
    case is_birthday_today birthday of
        Today ->
            birthday.age - 1

        Future ->
            birthday.age


smart_row_style : PlanetaryBirthday -> List (Attribute msg)
smart_row_style birthday =
    case is_birthday_today birthday of
        Today ->
            [ style "color" "yellow"
            , style "font-family" "cursive"
            , style "font-size" "105%"
            ]

        Future ->
            [ style "" "" ]


view_birthday : PlanetaryBirthday -> Html Msg
view_birthday birthday =
    let
        yearLocale =
            { usLocale
                | decimals = Exact 0
            }

        smartAge =
            smart_age birthday

        suffix =
            if smartAge == 1 then
                " yr"

            else
                " yrs"

        formatted_age =
            FormatNumber.format yearLocale (toFloat smartAge)
    in
    tr (smart_row_style birthday)
        [ td
            [ style "text-align" "left"
            , style "padding-left" "0.5em"
            ]
            [ text birthday.planet_name ]
        , td
            [ style "text-align" "center"
            , style "padding-right" "0.1em"
            ]
            (smart_birthday_message birthday)
        , td
            [ style "padding-left" "0.5em"
            , style "padding-right" "0.5em"
            ]
            [ span [] [ text formatted_age ]
            , smallSpacer
            , span [ style "font-size" "80%" ] [ text suffix ]
            , smallSpacer
            , span [ style "font-size" "80%" ] [ text "old" ]
            ]
        ]



-- VIEW


birthday_table_heading : List (Attribute msg)
birthday_table_heading =
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


month_options : ( Int, String ) -> Html Msg
month_options ( month_zero_indexed, string ) =
    option [ value (String.fromInt (month_zero_indexed + 1)) ] [ text string ]


day_option : Int -> Html msg
day_option day =
    option [ value (String.fromInt day) ] [ text (String.fromInt day) ]


day_options : Birthdate -> List (Html msg)
day_options birthdate =
    let
        year =
            Date.year birthdate

        day_max =
            case Date.month birthdate of
                Feb ->
                    if is_leap_year year then
                        29

                    else
                        28

                other ->
                    if List.member other [ Apr, Jun, Sep, Nov ] then
                        30

                    else
                        31
    in
    List.map day_option (List.range 1 day_max)


year_options : Int -> Html msg
year_options year =
    option [ value (String.fromInt year) ] [ text (String.fromInt year) ]


names_of_month : List String
names_of_month =
    [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" ]


date_inputs : Model -> Html Msg
date_inputs model =
    div []
        [ select
            [ style "font-size" "1.5em"
            , style "border-radius" ".2em"
            , onInput DayPicked
            , value (String.fromInt (Date.day model.user_birthdate))
            ]
            (day_options model.user_birthdate)
        , select
            [ style "font-size" "1.5em"
            , style "margin-left" ".3em"
            , style "margin-right" ".3em"
            , style "border-radius" ".2em"
            , onInput MonthPicked
            , value (String.fromInt (Date.monthNumber model.user_birthdate))
            ]
            (List.map month_options (List.indexedMap Tuple.pair names_of_month))

        , select
            [ style "font-size" "1.5em"
            , style "border-radius" ".2em"
            , onInput YearPicked
            , value (String.fromInt (Date.year model.user_birthdate))
            ]
            (List.map year_options (List.range 1 (Date.year model.today)))
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


compare_VIPs : ( String, String ) -> ( String, String ) -> Order
compare_VIPs a b =
    if Tuple.second a < Tuple.second b then
        LT

    else
        GT


view_VIP_link : ( String, String ) -> Html Msg
view_VIP_link ( path, name ) =
    li [ style "list-style-type" "none" ]
        [ a
            [ href ("?" ++ path)
            , style "text-decoration" "none"
            ]
            [ text name ]
        ]


celebration_texts : List String
celebration_texts =
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


view_celebration : String -> Html Msg
view_celebration suggestion =
    li [ style "list-style-type" "none" ] [ text suggestion ]


view : Model -> Browser.Document Msg
view model =
    { title = "9Birthdays"
    , body =
        [ div
            [ style "background-image" "url(solar-system.png)"
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
                    , src "logo.png"
                    , alt "Find your 9Birthdays"
                    , title "Find your 9Birthdays"
                    ]
                    []
                ]
            , h2 [ style "margin-top" "-1.8em" ] [ text "Born on this day?" ]
            , date_inputs model
            , h2
                [ style "margin-bottom" "0"
                , style "margin-top" "1em"
                ]
                [ text "Your next planetary birthdays are:" ]
            , table birthday_table_heading
                (List.map view_birthday <| List.sortWith Birthdays.compare model.birthdays)
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
                        (List.map view_VIP_link <| List.sortWith compare_VIPs vipLinks)
                    ]
                , div []
                    [ p [] [ text "How to celebrate your 9 planetary birthdays" ]
                    , ul [ style "font-size" "60%" ]
                        (List.map view_celebration celebration_texts)
                    ]
                ]
            , div footer
                [ span [ style "font-family" "Arial, sans-serif" ]
                    [ text "A small elm project, hosted by InfinityFree" ]
                ]
            ]
        ]
    }
