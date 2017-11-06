module Main exposing (main)

import Autocomplete
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Set exposing (Set)
import Task
import Time exposing (Time)
import Time.TimeZone exposing (TimeZone)
import Time.TimeZones as TZ
import Time.ZonedDateTime as ZDT


type alias Model =
    { rows : List TimeZone, autocomplete : Autocomplete.State, query : String, showMenu : Bool, now : Time }


type Msg
    = SetAutoState Autocomplete.Msg
    | SelectZone TimeZone
    | SetQuery String
    | GotTime Time


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SetAutoState Autocomplete.subscription


allZones =
    List.map (\x -> x ()) (Dict.values TZ.all)


init : Model
init =
    { rows = [], autocomplete = Autocomplete.empty, query = "", showMenu = False, now = 0 }


filterZones : String -> List TimeZone
filterZones query =
    List.filter (String.contains (normalizeString query) << normalizeString << Time.TimeZone.name) allZones


normalizeString : String -> String
normalizeString s =
    let
        underscoreToSpace : Char -> Char
        underscoreToSpace ch =
            case ch of
                '_' ->
                    ' '

                _ ->
                    ch
    in
    String.toLower s |> String.map underscoreToSpace


view : Model -> Html Msg
view model =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            Json.map
                (\code ->
                    if code == 38 || code == 40 then
                        Ok (SetQuery model.query)
                    else
                        Err "not handling that key"
                )
                keyCode
                |> Json.andThen
                    fromResult

        fromResult : Result String a -> Json.Decoder a
        fromResult result =
            case result of
                Ok val ->
                    Json.succeed val

                Err reason ->
                    Json.fail reason
    in
    div []
        [ h1 [] [text "Compare Working Hours"]
        , viewRows model.rows model.now
        , input
            [ onInput SetQuery

            --   , onFocus OnFocus
            --  , onWithOptions "keydown" options dec
            , class "autocomplete-input"
            , value model.query
            , placeholder "Add timezone"
            ]
            []
        , div [] []

        --  , text (toString model)
        , if model.showMenu then
            viewMenu model
          else
            div [] []
        ]


viewConfig : Autocomplete.ViewConfig TimeZone
viewConfig =
    let
        customizedLi keySelected mouseSelected tz =
            { attributes = [ classList [ ( "autocomplete-item", True ), ( "is-selected", keySelected || mouseSelected ) ] ]
            , children = [ Html.text (Time.TimeZone.name tz) ]
            }
    in
    Autocomplete.viewConfig
        { toId = Time.TimeZone.name
        , ul = [ class "autocomplete-list" ] -- set classes for your list
        , li = customizedLi -- given selection states and a person, create some Html!
        }


viewMenu : Model -> Html Msg
viewMenu model =
    div [ class "autocomplete-menu" ]
        [ Html.map SetAutoState (Autocomplete.view viewConfig 10 model.autocomplete (filterZones model.query)) ]


viewRows rows time =
    table [] (List.map (viewTimezone time) rows)


viewTimezone : Time -> TimeZone -> Html.Html msg
viewTimezone time r =
    let
        zdtNow =
            ZDT.fromTimestamp r time |> ZDT.setMinute 0

        hours =
            List.range 0 23

        label =
            td [ class "tz-label" ] [ text (Time.TimeZone.name r) ]

        hour zdt =
            ZDT.hour zdt |> toString |> (\x -> x ++ ":00")

        viewHour : Int -> Html.Html msg
        viewHour h =
            let
                hour =
                    ZDT.addHours h zdtNow |> ZDT.hour |> toString
            in
            td [ class "tz-hour", class ("hour-" ++ hour) ]
                [ text (hour ++ ":00") ]
    in
    tr [] (label :: List.map viewHour hours)


updateConfig : Autocomplete.UpdateConfig Msg TimeZone
updateConfig =
    Autocomplete.updateConfig
        { toId = Time.TimeZone.name
        , onKeyDown =
            \code maybeId ->
                if code == 13 then
                    Maybe.andThen TZ.fromName maybeId |> Maybe.map SelectZone
                else
                    Nothing
        , onTooLow = Nothing
        , onTooHigh = Nothing
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Maybe.map SelectZone <| TZ.fromName id
        , separateSelections = False
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            { model | query = newQuery, showMenu = True } ! []

        SelectZone zone ->
            { model | query = "", showMenu = False, rows = zone :: model.rows } ! []

        SetAutoState st ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update updateConfig st 10 model.autocomplete allZones

                newModel =
                    { model | autocomplete = newState }
            in
            case maybeMsg of
                Nothing ->
                    newModel ! []

                Just updateMsg ->
                    update updateMsg newModel

        GotTime t ->
            { model | now = t } ! []


main : Program Never Model Msg
main =
    Html.program
        { init = init ! [ Task.perform GotTime Time.now ]
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
