module App exposing (Model, Msg, app, monthNames)

{-| 9 Birthdays TEA

    Given a birthdate,
        immediately
            update UI with birthdays on solar system planets

-}

import Birthdays exposing (Birthdate, PlanetaryBirthday, Today)
import Browser
import Browser.Dom
import Browser.Navigation
import Char exposing (isAlpha)
import Css
import Date
import FormatNumber
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Html.Styled
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , h1
        , h2
        , img
        , li
        , option
        , p
        , select
        , span
        , table
        , td
        , text
        , tr
        , ul
        )
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events
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
    let
        init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
        init _ url key =
            let
                dummyTodayReplacedByAppStarted =
                    Date.fromCalendarDate 2022 May 1

                dummyBirthdaysReplacedByAppStarted =
                    []
            in
            ( { userBirthdate = fallbackBirthdate
              , birthdays = dummyBirthdaysReplacedByAppStarted
              , today = dummyTodayReplacedByAppStarted
              , key = key
              , url = url
              }
            , Task.perform AppStarted Date.today
            )
    in
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
    , key : Browser.Navigation.Key
    , url : Url.Url
    }



-- FUNCTIONS


isLeapYear : Year -> Bool
isLeapYear y =
    modBy 4 y == 0 && modBy 100 y /= 0 || modBy 400 y == 0


fallbackDay : number
fallbackDay =
    1


fallbackMonth : Month
fallbackMonth =
    Jan


fallbackMonthValue : number
fallbackMonthValue =
    1


fallbackYear : number
fallbackYear =
    2000


fallbackBirthdate : Birthdate
fallbackBirthdate =
    Date.fromCalendarDate fallbackYear fallbackMonth fallbackDay


updateBirthdateDay : Birthdate -> String -> Birthdate
updateBirthdateDay birthdate dayString =
    Date.fromCalendarDate
        (Date.year birthdate)
        (Date.month birthdate)
        (Maybe.withDefault fallbackDay (String.toInt dayString))


updateBirthdateMonth : Birthdate -> String -> Birthdate
updateBirthdateMonth birthdate monthString =
    Date.fromCalendarDate
        (Date.year birthdate)
        (Date.numberToMonth
            (Maybe.withDefault fallbackMonthValue (String.toInt monthString))
        )
        (Date.day birthdate)


updateBirthdateYear : Birthdate -> String -> Birthdate
updateBirthdateYear birthdate yearString =
    Date.fromCalendarDate
        (Maybe.withDefault fallbackYear (String.toInt yearString))
        (Date.month birthdate)
        (Date.day birthdate)


onBirthdatePicked : Model -> Birthdate -> ( Model, Cmd Msg )
onBirthdatePicked model birthdate =
    let
        correctedBirthdate =
            if Date.compare birthdate model.today == GT then
                -- any future birthdate is pulled back
                model.today

            else
                birthdate
    in
    ( { model
        | userBirthdate = correctedBirthdate
        , birthdays =
            Birthdays.calculateBirthdays
                correctedBirthdate
                model.today
      }
    , Browser.Navigation.pushUrl model.key
        ("?" ++ Date.toIsoString correctedBirthdate)
    )


onAppStarted : Model -> Today -> ( Model, Cmd Msg )
onAppStarted model newToday =
    let
        newBirthdate =
            birthdateFromUrl model.url newToday
    in
    ( { model
        | userBirthdate = newBirthdate
        , today = newToday
        , birthdays = Birthdays.calculateBirthdays newBirthdate newToday
      }
    , Cmd.none
    )


onChangeBirthdateUrl : Model -> Url.Url -> ( Model, Cmd Msg )
onChangeBirthdateUrl model url =
    let
        newBirthdate =
            birthdateFromUrl url model.today
    in
    ( { model
        | userBirthdate = newBirthdate
        , birthdays = Birthdays.calculateBirthdays newBirthdate model.today
      }
    , newTopPage url
    )


newTopPage : Url.Url -> Cmd Msg
newTopPage url =
    Task.perform (\_ -> ChangeUrl url) (Browser.Dom.setViewport 0 0)


onLinkedClicked : Model -> Browser.UrlRequest -> ( Model, Cmd Msg )
onLinkedClicked model urlRequest =
    case urlRequest of
        Browser.Internal url ->
            onChangeBirthdateUrl model url

        Browser.External href ->
            ( model
            , Browser.Navigation.load href
            )


onUrlChanged : Model -> Url.Url -> ( Model, Cmd Msg )
onUrlChanged model url =
    ( { model
        | userBirthdate = birthdateFromUrl url model.today
        , url = url
      }
    , Cmd.none
    )


onChangeUrl : Model -> Url.Url -> ( Model, Cmd Msg )
onChangeUrl model url =
    ( model
    , Browser.Navigation.pushUrl model.key (Url.toString url)
    )


onTryingElm : Model -> ( Model, Cmd Msg )
onTryingElm model =
    ( model
    , Browser.Navigation.load "/trying-elm.html"
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DayPicked dayString ->
            onBirthdatePicked
                model
                (updateBirthdateDay model.userBirthdate dayString)

        MonthPicked monthString ->
            onBirthdatePicked
                model
                (updateBirthdateMonth model.userBirthdate monthString)

        YearPicked yearString ->
            onBirthdatePicked
                model
                (updateBirthdateYear model.userBirthdate yearString)

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


{-| axiom: accept only birthdates from year 1 to today
-}
birthdateFromUrl : Url.Url -> Today -> Birthdate
birthdateFromUrl url today =
    case Maybe.map Date.fromIsoString url.query of
        Just (Ok birthdate) ->
            if
                Date.year birthdate
                    > 0
                    && Date.compare birthdate today
                    /= GT
            then
                birthdate

            else
                fallbackBirthdate

        _ ->
            fallbackBirthdate


type When
    = Today
    | Future


whenIsBirthday : PlanetaryBirthday -> When
whenIsBirthday birthday =
    if Date.compare birthday.earthDate birthday.todayOnEarth == EQ then
        Today

    else
        Future


smartBirthdayMessage : PlanetaryBirthday -> List (Html msg)
smartBirthdayMessage birthday =
    {-
       specical case: if today is a birthday, then
       while technically the next birthday is an orit away
       we really should shout out Happy Birthday today!
    -}
    let
        ordinalisedDate =
            Ordinal.ordinal (Date.format "d" birthday.earthDate)
                ++ Date.format " MMM y" birthday.earthDate
    in
    case whenIsBirthday birthday of
        Today ->
            [ span
                [ css [ Css.fontFamily Css.cursive ] ]
                [ text "Happy Birthday today!" ]
            ]

        Future ->
            [ text ordinalisedDate
            ]


viewBirthday : PlanetaryBirthday -> Html Msg
viewBirthday birthday =
    let
        smartRowStyle =
            case whenIsBirthday birthday of
                Today ->
                    [ css
                        [ Css.color backdropTextColor
                        , Css.fontFamily Css.cursive
                        , Css.fontSize (Css.pct 105)
                        ]
                    ]

                Future ->
                    []

        spanAtPercentage : Float -> String -> Html msg
        spanAtPercentage percentage content =
            span [ css [ Css.fontSize (Css.pct percentage) ] ] [ text content ]

        commaSeparated : Int -> String
        commaSeparated number =
            FormatNumber.format
                { usLocale
                    | decimals = Exact 0
                }
                (toFloat number)
    in
    tr smartRowStyle
        [ td
            [ css
                [ Css.textAlign Css.left
                , Css.paddingLeft (Css.em 0.5)
                ]
            ]
            [ text birthday.planetName ]
        , td
            [ css
                [ Css.textAlign Css.center
                , Css.paddingRight (Css.em 0.1)
                ]
            ]
            (smartBirthdayMessage birthday)
        , td
            [ css
                [ Css.paddingLeft (Css.em 0.5)
                , Css.paddingRight (Css.em 0.5)
                ]
            ]
            [ span [] [ text (commaSeparated birthday.age) ]
            , spanAtPercentage 40 " "
            , spanAtPercentage 80
                (if birthday.age == 1 then
                    " yr"

                 else
                    " yrs"
                )
            , spanAtPercentage 40 " "
            , spanAtPercentage 80 "old"
            ]
        ]



-- VIEW


birthdayTableHeading : List (Attribute msg)
birthdayTableHeading =
    [ css
        [ Css.textAlign Css.center
        , Css.backgroundColor (Css.rgba 50 50 50 0.7)
        , Css.borderRadius (Css.px 15)
        , Css.marginLeft Css.auto
        , Css.marginRight Css.auto
        , Css.marginTop (Css.em 1.8)
        ]
    ]


footer : Html Msg
footer =
    div
        [ css
            [ Css.fontSize (Css.pct 60)
            , Css.color textColor
            , Css.margin4 (Css.em 5) (Css.em 5) (Css.em 5) (Css.em 5)
            ]
        ]
        [ acknowledgements
        , myExperiences
        ]


monthOptions : ( Int, String ) -> Html Msg
monthOptions ( monthZeroIndexed, string ) =
    option
        [ Html.Styled.Attributes.value
            (String.fromInt (monthZeroIndexed + 1))
        ]
        [ text string ]


dayOption : Int -> Html msg
dayOption day =
    option
        [ Html.Styled.Attributes.value
            (String.fromInt day)
        ]
        [ text (String.fromInt day) ]


isInBirthdayMonth : Birthdate -> Today -> Bool
isInBirthdayMonth birthdate today =
    Date.month birthdate
        == Date.month today
        && Date.year birthdate
        == Date.year today


{-| set Day input to correct number of days for Month/Year picked
-}
dayOptions : Birthdate -> Today -> List (Html msg)
dayOptions birthdate today =
    let
        maxValidDay =
            min
                (if isInBirthdayMonth birthdate today then
                    Date.day today

                 else
                    31
                )
                (case Date.month birthdate of
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
                )
    in
    List.map dayOption (List.range 1 maxValidDay)


yearOptions : Int -> Html msg
yearOptions year =
    option
        [ Html.Styled.Attributes.value (String.fromInt year)
        ]
        [ text (String.fromInt year) ]


namesOfMonth : Month -> String
namesOfMonth month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


validMonth : Today -> Year -> Month -> Bool
validMonth today year month =
    if year == Date.year today then
        Date.monthToNumber month <= Date.monthNumber today

    else
        True


months : List Month
months =
    [ Jan
    , Feb
    , Mar
    , Apr
    , May
    , Jun
    , Jul
    , Aug
    , Sep
    , Oct
    , Nov
    , Dec
    ]


monthNames : Year -> Today -> List String
monthNames year today =
    List.map namesOfMonth (List.filter (validMonth today year) months)


selectDateCss : List Css.Style
selectDateCss =
    [ Css.fontSize (Css.em 1.5)
    , Css.borderRadius (Css.em 0.2)
    ]


dateInputs : Model -> Html Msg
dateInputs model =
    div []
        [ select
            [ css
                selectDateCss
            , Html.Styled.Events.onInput DayPicked
            , Html.Styled.Attributes.value
                (String.fromInt (Date.day model.userBirthdate))
            ]
            (dayOptions model.userBirthdate model.today)
        , select
            [ css
                (selectDateCss
                    ++ [ Css.marginLeft (Css.em 0.3)
                       , Css.marginRight (Css.em 0.3)
                       ]
                )
            , Html.Styled.Events.onInput MonthPicked
            , Html.Styled.Attributes.value
                (String.fromInt (Date.monthNumber model.userBirthdate))
            ]
            (List.map monthOptions
                (List.indexedMap Tuple.pair
                    (monthNames (Date.year model.userBirthdate) model.today)
                )
            )
        , select
            [ css
                selectDateCss
            , Html.Styled.Events.onInput YearPicked
            , Html.Styled.Attributes.value
                (String.fromInt (Date.year model.userBirthdate))
            ]
            (List.map yearOptions <|
                List.reverse <|
                    List.range 1 (Date.year model.today)
            )
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
    if
        String.filter isAlpha (Tuple.second a)
            < String.filter isAlpha (Tuple.second b)
    then
        LT

    else
        GT


viewVIPLink : ( String, String ) -> Html Msg
viewVIPLink ( path, name ) =
    li [ css [ Css.listStyleType Css.none ] ]
        [ a
            [ Html.Styled.Attributes.href ("?" ++ path)
            , css [ Css.textDecoration Css.none ]
            ]
            [ text name ]
        ]


celebrationTexts : List String
celebrationTexts =
    [ """On Mercury, spend sometime taking in the countryside"""
    , """For a Venusian birthday, make a new favourite meal - no cardboard 
    containers, please"""
    , """On homely Earth, there is so much to celebrate and so much to do - try 
    doing it with cake!!"""
    , """Grasp your Mars anniversary and go see a play or movie"""
    , """Jupiter birthdays are precious and rare - do something for the first 
    time!"""
    , """Saturn starts a weekend whenever it falls so make time to celebrate 
    with a friend"""
    , """Find one of the great inventors from history and mark their Uranus 
    birthday with panache!"""
    , """Celebrate a great artist's Neptune birthday and do it wearing a hat 
    :)"""
    , """Make your mark, here on planet Earth, and have the globe celebrate 
    your first Plutonian birthday!!"""
    ]


viewCelebration : String -> Html Msg
viewCelebration suggestion =
    li [ css [ Css.listStyleType Css.none ] ] [ text suggestion ]


black : Css.Color
black =
    Css.rgb 0 0 0


white : Css.Color
white =
    Css.rgb 255 255 255


textColor : Css.Color
textColor =
    black


backgroundColor : Css.Color
backgroundColor =
    white


backdropTextColor : Css.Color
backdropTextColor =
    white


hyperlinkTextColor : Css.Color
hyperlinkTextColor =
    Css.hex "#3894FF"


backdropStyle : List (Attribute msg)
backdropStyle =
    [ css
        [ Css.backgroundImage (Css.url "/solar-system.png")
        , Css.backgroundRepeat Css.noRepeat
        , Css.width (Css.px 1000)
        , Css.color backdropTextColor
        , Css.textAlign Css.center
        , Css.margin4 (Css.em 0.5) (Css.em 0.5) (Css.em 0.5) (Css.em 0.5)
        , Css.fontFamilies [ "Arial", "sans-serif" ]
        , Css.fontSize (Css.em 1.5)
        ]
    ]


logoBorder : List Css.Style
logoBorder =
    [ Css.borderRadius (Css.px 15)
    , Css.backgroundColor black
    , Css.padding4 (Css.em 0.3) (Css.em 0.3) (Css.em 0.3) (Css.em 0.3)
    , Css.marginTop (Css.em -1.1)
    ]


logo : Html msg
logo =
    h1
        [ css [ Css.fontSize (Css.em 3) ] ]
        [ img
            [ css logoBorder
            , Html.Styled.Attributes.src "/logo.png"
            , Html.Styled.Attributes.alt "Find your 9Birthdays"
            , Html.Styled.Attributes.title "Find your 9Birthdays"
            ]
            []
        ]


prompt : Html msg
prompt =
    h2 [ css [ Css.marginTop (Css.em -1.8) ] ] [ text "Born on this day?" ]


birthdaysTitle : Html msg
birthdaysTitle =
    h2
        [ css
            [ Css.marginBottom Css.zero
            , Css.marginTop (Css.em 1)
            ]
        ]
        [ text "Your next planetary birthdays are:" ]


birthdays : Model -> Html Msg
birthdays model =
    table birthdayTableHeading
        (List.map viewBirthday
            (List.sortWith Birthdays.compare model.birthdays)
        )


supplementaries : Html Msg
supplementaries =
    div
        [ css
            [ Css.color textColor
            , Css.paddingTop (Css.em 3)
            ]
        ]
        [ insightMessage
        , vipHeadings
        , howToCelebrate
        ]


insightMessage : Html msg
insightMessage =
    div
        [ css
            [ Css.marginTop (Css.em 9)
            , Css.paddingBottom Css.zero
            , Css.marginBottom Css.zero
            ]
        ]
        [ p []
            [ text
                """Did you know we have birthdays on each planet in our solar 
                system?"""
            ]
        , p
            [ css
                [ Css.fontSize (Css.pct 70)
                , Css.marginTop (Css.em -1)
                ]
            ]
            [ text
                """It's true, enter your birthdate above and we'll calcuate 
                your 9 birthdays"""
            ]
        ]


vipHeadings : Html Msg
vipHeadings =
    div
        [ css [ Css.paddingTop (Css.em 0.2) ]
        ]
        [ span [] [ text "A few examples" ]
        , ul [ css [ Css.fontSize (Css.pct 75) ] ]
            (List.map viewVIPLink
                (List.sortWith compareVIPs vipLinks)
            )
        ]


howToCelebrate : Html Msg
howToCelebrate =
    div []
        [ p [] [ text "How to celebrate your 9 planetary birthdays" ]
        , ul [ css [ Css.fontSize (Css.pct 60) ] ]
            (List.map viewCelebration celebrationTexts)
        ]


acknowledgements : Html msg
acknowledgements =
    div
        []
        [ text "A small elm project, hosted by InfinityFree" ]



{-
   use button to "hyperlink" out of app
   true [a] tags reserved for LinkedClicked msgs
   this avoids parsing types of internal URLs
-}


myExperiences : Html Msg
myExperiences =
    div
        [ css
            [ Css.marginTop (Css.em 0.2)
            ]
        ]
        [ button
            [ Html.Styled.Events.onClick TryingElm
            , css
                [ Css.textDecoration Css.underline
                , Css.cursor Css.pointer
                , Css.border Css.zero
                , Css.color hyperlinkTextColor
                , Css.backgroundColor backgroundColor
                , Css.padding (Css.em 0.6)
                , Css.borderRadius (Css.px 5)
                ]
            ]
            [ text "My experiences trying Elm" ]
        ]


page : Model -> Html Msg
page model =
    div
        backdropStyle
        [ logo
        , prompt
        , dateInputs model
        , birthdaysTitle
        , birthdays model
        , supplementaries
        , footer
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "9Birthdays"
    , body = [ Html.Styled.toUnstyled (page model) ]
    }
