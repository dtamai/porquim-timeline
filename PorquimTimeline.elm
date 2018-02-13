module PorquimTimeline exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (spanishLocale)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }



-- MODEL


type alias Snapshot =
    { referenceDate : String
    , entries : List SnapshotEntry
    }


type alias SnapshotEntry =
    { bucketName : String
    , value : Float
    }


type alias Model =
    { prevSnapshots : List Snapshot
    , currentSnapshot : Snapshot
    , nextSnapshots : List Snapshot
    }


initialModel : Model
initialModel =
    Model
        [ (Snapshot "Dez/17"
            [ SnapshotEntry "Bucket A" 1123.45
            , SnapshotEntry "Bucket B" 2234.56
            ]
          )
        ]
        (Snapshot "Jan/18"
            [ SnapshotEntry "Bucket A" 1234.56
            , SnapshotEntry "Bucket B" 2345.67
            ]
        )
        [ (Snapshot "Fev/18"
            [ SnapshotEntry "Bucket A" 1345.67
            , SnapshotEntry "Bucket B" 2456.78
            ]
          )
        ]


snapshotTotal : List SnapshotEntry -> Float
snapshotTotal entries =
    entries
        |> List.map .value
        |> List.sum


prevSnapshot : Model -> Maybe Snapshot
prevSnapshot model =
    List.head model.prevSnapshots


nextSnapshot : Model -> Maybe Snapshot
nextSnapshot model =
    List.head model.nextSnapshots



-- UPDATE


type Msg
    = PrevSnapshot
    | NextSnapshot


moveToPrevSnapshot : Model -> Model
moveToPrevSnapshot model =
    case prevSnapshot model of
        Just snapshot ->
            { model
                | prevSnapshots = List.drop 1 model.prevSnapshots
                , currentSnapshot = snapshot
                , nextSnapshots = model.currentSnapshot :: model.nextSnapshots
            }

        Nothing ->
            model


moveToNextSnapshot : Model -> Model
moveToNextSnapshot model =
    case nextSnapshot model of
        Just snapshot ->
            { model
                | prevSnapshots = model.currentSnapshot :: model.prevSnapshots
                , currentSnapshot = snapshot
                , nextSnapshots = List.drop 1 model.nextSnapshots
            }

        Nothing ->
            model


update : Msg -> Model -> Model
update msg model =
    case msg of
        PrevSnapshot ->
            moveToPrevSnapshot model

        NextSnapshot ->
            moveToNextSnapshot model



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewSnapshot model.currentSnapshot
        , viewControls model
        ]


viewSnapshot : Snapshot -> Html Msg
viewSnapshot snapshot =
    section [ class "current-snapshot" ]
        [ h2 [] [ text snapshot.referenceDate ]
        , viewSnapshotEntries snapshot.entries
        ]


viewSnapshotEntries : List SnapshotEntry -> Html Msg
viewSnapshotEntries entries =
    let
        header =
            thead [ class "thead-dark" ]
                [ th [ class "bucket-name" ] [ text "Bucket" ]
                , th [ class "value" ] [ text "Valor" ]
                ]

        body =
            tbody [] (entries |> List.map viewSnapshotEntryItem)

        footer =
            tfoot [] [ viewSnapshotTotal <| snapshotTotal entries ]
    in
        div []
            [ table [ class "table table-sm table-striped" ]
                [ header
                , body
                , footer
                ]
            ]


viewSnapshotEntryItem : SnapshotEntry -> Html Msg
viewSnapshotEntryItem entry =
    tr []
        [ td [ class "bucket-name" ] [ text entry.bucketName ]
        , td [ class "value" ] [ text <| format entry.value ]
        ]


viewSnapshotTotal : Float -> Html Msg
viewSnapshotTotal total =
    tr [ class "table-info" ]
        [ td [ class "bucket-name" ] [ text "Total" ]
        , td [ class "value" ] [ text <| format total ]
        ]


viewControls : Model -> Html Msg
viewControls model =
    div [ class "controls" ]
        [ viewControlPrev model
        , viewControlNext model
        ]


viewControlPrev : Model -> Html Msg
viewControlPrev model =
    case prevSnapshot model of
        Just snapshot ->
            a [ href "#", onClick PrevSnapshot ]
                [ text <| "« " ++ snapshot.referenceDate ]

        Nothing ->
            div [] []


viewControlNext : Model -> Html Msg
viewControlNext model =
    case nextSnapshot model of
        Just snapshot ->
            a [ href "#", onClick NextSnapshot ]
                [ text <| snapshot.referenceDate ++ " »" ]

        Nothing ->
            div [] []


locale : FormatNumber.Locales.Locale
locale =
    { spanishLocale | decimals = 2 }


format : Float -> String
format number =
    FormatNumber.format locale number
