module PorquimTimeline exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


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


snapshotTotal : Snapshot -> Float
snapshotTotal snapshot =
    snapshot.entries
        |> List.map .value
        |> List.sum


prevSnapshot : Model -> Model
prevSnapshot model =
    case List.head model.prevSnapshots of
        Just snapshot ->
            { model
                | prevSnapshots = List.drop 1 model.prevSnapshots
                , currentSnapshot = snapshot
                , nextSnapshots = model.currentSnapshot :: model.nextSnapshots
            }

        Nothing ->
            model


nextSnapshot : Model -> Model
nextSnapshot model =
    case List.head model.nextSnapshots of
        Just snapshot ->
            { model
                | prevSnapshots = model.currentSnapshot :: model.prevSnapshots
                , currentSnapshot = snapshot
                , nextSnapshots = List.drop 1 model.nextSnapshots
            }

        Nothing ->
            model



-- UPDATE


type Msg
    = PrevSnapshot
    | NextSnapshot


update : Msg -> Model -> Model
update msg model =
    case msg of
        PrevSnapshot ->
            prevSnapshot model

        NextSnapshot ->
            nextSnapshot model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewSnapshot model.currentSnapshot
        , viewControls
        ]


viewSnapshot : Snapshot -> Html Msg
viewSnapshot snapshot =
    section []
        [ div [] [ text snapshot.referenceDate ]
        , viewSnapshotEntries snapshot.entries
        , viewSnapshotTotal (snapshotTotal snapshot)
        ]


viewSnapshotEntries : List SnapshotEntry -> Html Msg
viewSnapshotEntries entries =
    ul [] (List.map viewSnapshotEntryItem entries)


viewSnapshotEntryItem : SnapshotEntry -> Html Msg
viewSnapshotEntryItem entry =
    li []
        [ span [] [ text entry.bucketName ]
        , span [] [ text "  -  " ]
        , span [] [ text (toString entry.value) ]
        ]


viewSnapshotTotal : Float -> Html Msg
viewSnapshotTotal total =
    div []
        [ text ("Total  -  " ++ toString total) ]


viewControls : Html Msg
viewControls =
    div []
        [ a [ href "#", onClick PrevSnapshot ] [ text "«" ]
        , a [ href "#", onClick NextSnapshot ] [ text "»" ]
        ]
