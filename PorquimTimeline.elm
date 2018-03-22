module PorquimTimeline exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
    , entries : Array SnapshotEntry
    }


type alias SnapshotEntry =
    { bucketName : String
    , value : String
    }


setBucketName : String -> SnapshotEntry -> SnapshotEntry
setBucketName bucketName entry =
    { entry | bucketName = bucketName }


setEntryValue : String -> SnapshotEntry -> SnapshotEntry
setEntryValue value entry =
    { entry | value = value }


type Operation
    = ShowSnapshot
    | AddNewSnapshot


type alias Model =
    { prevSnapshots : List Snapshot
    , currentSnapshot : Snapshot
    , nextSnapshots : List Snapshot
    , newSnapshot : Snapshot
    , operation : Operation
    }


initialModel : Model
initialModel =
    Model
        [ (Snapshot
            "Dez/17"
            (Array.fromList
                [ SnapshotEntry "Bucket A" "1123.45"
                , SnapshotEntry "Bucket B" "2234.56"
                ]
            )
          )
        ]
        (Snapshot
            "Jan/18"
            (Array.fromList
                [ SnapshotEntry "Bucket A" "1234.56"
                , SnapshotEntry "Bucket B" "2345.67"
                ]
            )
        )
        [ (Snapshot
            "Fev/18"
            (Array.fromList
                [ SnapshotEntry "Bucket A" "1345.67"
                , SnapshotEntry "Bucket B" "2456.78"
                ]
            )
          )
        ]
        (Snapshot "" Array.empty)
        ShowSnapshot


snapshotTotal : List SnapshotEntry -> Float
snapshotTotal entries =
    entries
        |> List.map (\e -> Result.withDefault 0 (String.toFloat e.value))
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
    | EnterNewSnapshot
    | LeaveNewSnapshot
    | EntryRefDateInput String
    | EntryBucketInput Int String
    | EntryValueInput Int String


moveToPrevSnapshot : Model -> Model
moveToPrevSnapshot model =
    case prevSnapshot model of
        Just snapshot ->
            { model
                | prevSnapshots = List.drop 1 model.prevSnapshots
                , currentSnapshot = snapshot
                , nextSnapshots = model.currentSnapshot :: model.nextSnapshots
                , operation = ShowSnapshot
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
                , operation = ShowSnapshot
            }

        Nothing ->
            model


openNewSnapshotForm : Model -> Model
openNewSnapshotForm model =
    { model | operation = AddNewSnapshot }


closeNewSnapshotForm : Model -> Model
closeNewSnapshotForm model =
    { model | operation = ShowSnapshot }


update : Msg -> Model -> Model
update msg model =
    case msg of
        PrevSnapshot ->
            moveToPrevSnapshot model

        NextSnapshot ->
            moveToNextSnapshot model

        EnterNewSnapshot ->
            openNewSnapshotForm model

        LeaveNewSnapshot ->
            closeNewSnapshotForm model

        EntryRefDateInput refDate ->
            let
                newSnapshot =
                    model.newSnapshot

                newNewSnapshot =
                    { newSnapshot | referenceDate = refDate }
            in
                { model | newSnapshot = newNewSnapshot }

        EntryBucketInput entryIdx bucketName ->
            let
                newSnapshot =
                    model.newSnapshot

                entry =
                    Array.get entryIdx newSnapshot.entries

                newEntries =
                    case entry of
                        Just entry ->
                            Array.set entryIdx (entry |> setBucketName bucketName) newSnapshot.entries

                        Nothing ->
                            Array.push (SnapshotEntry bucketName "0") newSnapshot.entries

                newNewSnapshot =
                    { newSnapshot
                        | entries = newEntries
                    }
            in
                { model | newSnapshot = newNewSnapshot }

        EntryValueInput entryIdx value ->
            let
                newSnapshot =
                    model.newSnapshot

                entry =
                    Array.get entryIdx newSnapshot.entries

                newEntries =
                    case entry of
                        Just entry ->
                            Array.set entryIdx (entry |> setEntryValue value) newSnapshot.entries

                        Nothing ->
                            Array.push (SnapshotEntry "" value) newSnapshot.entries

                newNewSnapshot =
                    { newSnapshot
                        | entries = newEntries
                    }
            in
                { model | newSnapshot = newNewSnapshot }



-- VIEW


view : Model -> Html Msg
view model =
    let
        mainSectionView =
            case model.operation of
                ShowSnapshot ->
                    [ viewSnapshot model.currentSnapshot
                    , viewControls <| viewControlsPrevNext model
                    ]

                AddNewSnapshot ->
                    [ viewSnapshotForm model.newSnapshot
                    , viewControls viewControlCancelNewSnapshot
                    ]
    in
        div [ class "container" ]
            mainSectionView


viewSnapshot : Snapshot -> Html Msg
viewSnapshot snapshot =
    section [ class "snapshot" ]
        [ h2 [ class "snapshot--ref" ] [ text snapshot.referenceDate ]
        , viewSnapshotEntries (Array.toList snapshot.entries)
        ]


viewSnapshotForm : Snapshot -> Html Msg
viewSnapshotForm snapshot =
    section [ class "snapshot" ]
        [ div [ class "snapshot--ref" ]
            [ input
                [ type_ "text"
                , class "form-control snapshot--ref__input"
                , placeholder "Ref."
                , onInput EntryRefDateInput
                ]
                []
            ]
        , viewSnapshotFormEntries snapshot
        ]


viewSnapshotFormEntries : Snapshot -> Html Msg
viewSnapshotFormEntries snapshot =
    let
        entry =
            Maybe.withDefault (SnapshotEntry "" "") (Array.get 0 snapshot.entries)

        value_ =
            if entry.value == "0" then
                ""
            else
                entry.value

        header =
            thead [ class "thead-dark" ]
                [ th [ class "bucket-name" ] [ text "Bucket" ]
                , th [ class "value" ] [ text "Valor" ]
                ]

        body =
            tbody []
                [ tr []
                    [ td []
                        [ input
                            [ type_ "text"
                            , class "form-control bucket-name"
                            , onInput (EntryBucketInput 0)
                            , value entry.bucketName
                            ]
                            []
                        ]
                    , td []
                        [ input
                            [ type_ "text"
                            , class "form-control value"
                            , placeholder "R$"
                            , onInput (EntryValueInput 0)
                            , value value_
                            ]
                            []
                        ]
                    ]
                ]
    in
        table [ class "table table-sm table-striped" ]
            [ header
            , body
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
        , td [ class "value" ] [ text <| format (toString total) ]
        ]


viewControls : List (Html Msg) -> Html Msg
viewControls controls =
    div [ class "controls" ]
        controls


viewControlsPrevNext : Model -> List (Html Msg)
viewControlsPrevNext model =
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
            a [ href "#", onClick EnterNewSnapshot ]
                [ text "Novo" ]


viewControlCancelNewSnapshot : List (Html Msg)
viewControlCancelNewSnapshot =
    [ a [ href "#", onClick LeaveNewSnapshot ]
        [ text "Cancelar" ]
    ]


locale : FormatNumber.Locales.Locale
locale =
    { spanishLocale | decimals = 2 }


format : String -> String
format number =
    case String.toFloat number of
        Ok value ->
            FormatNumber.format locale value

        Err _ ->
            "N/A"
