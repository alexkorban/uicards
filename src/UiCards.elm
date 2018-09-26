module UiCards exposing (card, cardError, deck, show)

import Array exposing (Array)
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (attribute, href, style)
import Html.Events exposing (onClick)
import Url exposing (Url)


type alias Card msg model =
    { name : String, model : model, view : model -> Html msg }


type alias CardIndex =
    ( Int, Int )


type alias FullCard msg model =
    { name : String, model : model, view : model -> Html msg, index : CardIndex }


type alias Deck msg model =
    { name : String, cards : List (Card msg model) }


type alias FullDeck msg model =
    { name : String, cards : Array (FullCard msg model) }


type alias CardModel msg model =
    Array (FullDeck msg model)


type alias AppUpdate msg model =
    msg -> model -> ( model, Cmd msg )


type InternalMsg
    = ToggleMenu
    | ClickLink UrlRequest
    | ChangeUrl Url


type alias CardMsg msg =
    ( CardIndex, msg )


type Msg msg
    = CardMsg (CardMsg msg)
    | InternalMsg InternalMsg


type alias InternalModel =
    { isMenuOpen : Bool
    , navKey : Nav.Key
    , selectedDeckIndex : Maybe Int
    }


type alias Model msg model =
    { internal : InternalModel
    , decks : CardModel msg model
    }



-- HELPERS --


flip : (a -> b -> c) -> b -> a -> c
flip givenF givenArg2 givenArg1 =
    givenF givenArg1 givenArg2


applyPair : (a -> b -> c) -> ( a, b ) -> c
applyPair givenF givenPair =
    givenF (Tuple.first givenPair) (Tuple.second givenPair)


toFullCard : Int -> Int -> Card msg model -> FullCard msg model
toFullCard givenDeckIndex givenCardIndex givenCard =
    FullCard givenCard.name givenCard.model givenCard.view ( givenDeckIndex, givenCardIndex )


toFullDeck : Int -> Deck msg model -> FullDeck msg model
toFullDeck givenIndex { name, cards } =
    let
        fullCards =
            Array.fromList <| List.indexedMap (toFullCard givenIndex) cards
    in
    FullDeck name fullCards



-- INIT --


init : List (Deck msg model) -> () -> Url -> Nav.Key -> ( Model msg model, Cmd (Msg msg) )
init givenDecks _ givenUrl givenNavKey =
    let
        cardModel =
            Debug.log "initial card model" Array.fromList <| List.indexedMap toFullDeck givenDecks
    in
    ( { internal =
            { isMenuOpen = False
            , navKey = givenNavKey
            , selectedDeckIndex = givenUrl.fragment |> Maybe.andThen String.toInt
            }
      , decks = cardModel
      }
    , Cmd.none
    )



-- UPDATE --


update : AppUpdate msg model -> Msg msg -> Model msg model -> ( Model msg model, Cmd (Msg msg) )
update givenAppUpdate givenMsg givenModel =
    case givenMsg of
        InternalMsg internalMsg ->
            let
                ( internalModel, internalCmd ) =
                    internalUpdate internalMsg givenModel.internal
            in
            ( { givenModel | internal = internalModel }, Cmd.map InternalMsg internalCmd )

        CardMsg cardMsg ->
            let
                ( cardModel, cardCmd ) =
                    cardUpdate givenAppUpdate cardMsg givenModel.decks
            in
            ( { givenModel | decks = cardModel }, Cmd.map CardMsg cardCmd )


internalUpdate : InternalMsg -> InternalModel -> ( InternalModel, Cmd InternalMsg )
internalUpdate givenMsg givenModel =
    case givenMsg of
        ChangeUrl url ->
            ( { givenModel | selectedDeckIndex = url.fragment |> Maybe.andThen String.toInt }, Cmd.none )

        ClickLink givenUrlRequest ->
            case givenUrlRequest of
                Internal url ->
                    ( givenModel, Nav.pushUrl givenModel.navKey <| Url.toString url )

                External url ->
                    ( givenModel, Nav.load url )

        ToggleMenu ->
            ( { givenModel | isMenuOpen = not givenModel.isMenuOpen }, Cmd.none )


cardUpdate : AppUpdate msg model -> CardMsg msg -> CardModel msg model -> ( CardModel msg model, Cmd (CardMsg msg) )
cardUpdate givenAppUpdate givenCardMsg givenCardModel =
    let
        ( ( deckIndex, cardIndex ), cardMsg ) =
            givenCardMsg

        updateCard givenCard =
            let
                ( newModel, cmd ) =
                    givenAppUpdate cardMsg givenCard.model
            in
            ( { givenCard | model = newModel }, Cmd.map (\cmdMsg -> ( ( deckIndex, cardIndex ), cmdMsg )) cmd )
    in
    case Array.get deckIndex givenCardModel of
        Nothing ->
            ( givenCardModel, Cmd.none )

        Just givenDeck ->
            case Array.get cardIndex givenDeck.cards of
                Nothing ->
                    ( givenCardModel, Cmd.none )

                Just givenCard ->
                    let
                        ( newCard, cmd ) =
                            updateCard givenCard

                        updatedDeck =
                            { givenDeck | cards = Array.set cardIndex newCard givenDeck.cards }

                        newCardModel =
                            Array.set deckIndex updatedDeck givenCardModel
                    in
                    ( newCardModel, cmd )



-- SUBSCRIPTIONS --


subscriptions : Model msg model -> Sub (Msg msg)
subscriptions _ =
    Sub.none



-- VIEW --


toAttrs =
    List.map (applyPair style)


type alias UrlString =
    String


type alias NamedUrl =
    { url : UrlString, name : String }


menuPanel : Array NamedUrl -> Html (Msg msg)
menuPanel givenLinks =
    let
        bgStyles =
            [ ( "height", "100vh" )
            , ( "width", "100vw" )
            , ( "position", "fixed" )
            , ( "top", "0" )
            , ( "left", "0" )
            , ( "background-color", "rgba(0, 0, 0, 0.5)" )
            ]

        styles =
            [ ( "max-width", "80%" )
            , ( "min-width", "200px" )
            , ( "height", "100vh" )
            , ( "position", "fixed" )
            , ( "top", "0" )
            , ( "right", "0" )
            , ( "border-left", "2px solid #5e53a3" )
            , ( "background-color", "#fffaf5" )
            ]

        listStyles =
            [ ( "list-style", "none" ) ]

        toLi namedUrl =
            let
                linkStyles =
                    [ ( "text-decoration", "none" )
                    , ( "font-family", "Helvetica, serif" )
                    , ( "color", "#292446" )
                    ]

                attrs =
                    [ href namedUrl.url ] ++ toAttrs linkStyles
            in
            li [ style "margin-top" "10px" ] [ a attrs [ Html.text namedUrl.name ] ]
    in
    div (toAttrs bgStyles ++ [ onClick <| InternalMsg ToggleMenu ])
        [ div (toAttrs styles)
            [ ul (toAttrs listStyles) <|
                Array.toList <|
                    Array.map toLi givenLinks
            ]
        ]


view : Model msg model -> Document (Msg msg)
view givenModel =
    let
        cardWrapperStyles =
            [ ( "border-radius", "10px 10px 0 0" )
            , ( "box-shadow", "0px 0px 1px 1px #9381ff" )
            ]

        cardHeadingStyles =
            [ ( "box-sizing", "border-box" )
            , ( "margin", "30px 0 0 0" )
            , ( "padding", "10px 10px 8px 10px" )
            , ( "border-radius", "10px 10px 0 0" )
            , ( "background-color", "#d8d8ff" )
            , ( "color", "#292446" )
            , ( "font-family", "Helvetica, sans-serif" )
            , ( "font-size", "1em" )
            , ( "font-weight", "bold" )
            ]

        cardStyles =
            [ ( "border", "3px solid #d8d8ff" )
            , ( "border-top", "none" )
            , ( "background-color", "#fff" )
            ]

        cardLinerStyles =
            [ ( "border", "1px solid #c2c5ff" )
            , ( "padding", "5px" )
            ]

        viewCard givenCard =
            div (toAttrs cardWrapperStyles)
                [ h2 (toAttrs cardHeadingStyles) [ text <| String.toUpper givenCard.name ]
                , div (toAttrs cardStyles)
                    [ div (toAttrs cardLinerStyles)
                        [ Html.map (\givenMsg -> CardMsg ( givenCard.index, givenMsg )) <| givenCard.view givenCard.model ]
                    ]
                ]

        mainStyles =
            [ ( "width", "100%" )
            , ( "box-sizing", "border-box" )
            , ( "padding", "0px 10px 50px 10px" )
            , ( "margin-top", "-10px" )
            ]

        viewCards givenCards =
            node "main" (toAttrs mainStyles) <| List.map viewCard <| Array.toList givenCards

        headerStyles =
            [ ( "padding", "10px" )
            , ( "background-color", "#ffeedd" )
            , ( "border-bottom", "1px solid #e8d9c9" )
            , ( "line-height", "45px" )
            ]

        deckHeadingStyles =
            [ ( "font-family", "Helvetica, sans-serif" )
            , ( "font-variant", "small-caps" )
            , ( "letter-spacing", ".1rem" )
            , ( "color", "#292446" )
            , ( "display", "inline-block" )
            , ( "margin", "0" )
            , ( "line-height", "45px" )
            ]

        menuButtonStyles =
            [ ( "padding", "10px" )
            , ( "height", "45px" )
            , ( "border", "2px solid #b8b8ff" )
            , ( "border-radius", "4px" )
            , ( "background-color", "#f8f7ff" )
            , ( "color", "#292446" )
            , ( "font-weight", "bold" )
            , ( "font-size", "1.05em" )
            , ( "float", "right" )
            , ( "box-shadow", "0 0 1px 1px #9381ff" )
            ]

        viewDeck givenDeck =
            [ header (toAttrs headerStyles)
                [ h1 (toAttrs deckHeadingStyles) [ text givenDeck.name ]
                , button (toAttrs menuButtonStyles ++ [ onClick <| InternalMsg ToggleMenu ]) [ text "Decks" ]
                ]
            , viewCards givenDeck.cards
            ]

        toNamedUrl givenIndex givenDeck =
            { url = "#" ++ String.fromInt givenIndex, name = givenDeck.name }

        maybeDeck =
            givenModel.internal.selectedDeckIndex
                |> Maybe.andThen (flip Array.get givenModel.decks)

        body =
            case maybeDeck of
                Just currDeck ->
                    [ node "style" [] [ text "html, body { margin: 0; background-color: #f8f7ff }" ] ] ++ viewDeck currDeck

                Nothing ->
                    [ h1 [] [ text "Invalid deck index" ] ]

        filledMenuPanel =
            menuPanel <| Array.indexedMap toNamedUrl givenModel.decks
    in
    { title = "UICards"
    , body =
        if givenModel.internal.isMenuOpen then
            body ++ [ filledMenuPanel ]

        else
            body
    }



-- PUBLIC API --


card =
    Card


deck =
    Deck


cardError : String -> Html.Html msg
cardError givenErr =
    let
        errorStyles =
            [ ( "color", "#cc0000" )
            , ( "font-family", "Helvetica, sans-serif" )
            , ( "font-weight", "bold" )
            ]
    in
    div (toAttrs errorStyles) [ text givenErr ]


show : AppUpdate msg model -> List (Deck msg model) -> Program () (Model msg model) (Msg msg)
show givenAppUpdate givenDecks =
    Browser.application
        { init = init givenDecks
        , update = update givenAppUpdate
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = InternalMsg << ClickLink
        , onUrlChange = InternalMsg << ChangeUrl
        }
