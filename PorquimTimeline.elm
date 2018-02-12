module PorquimTimeline exposing (..)

import Html exposing (..)


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
    { currentSnapshot : Snapshot }


initialModel : Model
initialModel =
    Model
        (Snapshot "Jan/18"
            [ SnapshotEntry "Bucket A" 1234.56
            , SnapshotEntry "Bucket B" 2345.67
            ]
        )


snapshotTotal : Snapshot -> Float
snapshotTotal snapshot =
    snapshot.entries
        |> List.map .value
        |> List.sum



-- UPDATE


type Msg
    = None


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewSnapshot model.currentSnapshot
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
