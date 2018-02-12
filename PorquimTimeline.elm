module PorquimTimeline exposing (..)

import Html exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = {}
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    {}



-- UPDATE


type Msg
    = None


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW


view : Model -> Html Msg
view model =
    h1 [] [ text "Porquim Timeline" ]
