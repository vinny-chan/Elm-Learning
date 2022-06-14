module Todo exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Task =
    { id : Int, content : String, isComplete : Bool }


type alias Model =
    { tasks : List Task, inputValue : String, nextId : Int }


initialModel : Model
initialModel =
    Model [] "" 1


type Msg
    = AddTask
    | ChangeInput String
    | DeleteTask Int
    | ToggleCheckbox Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTask ->
            { model | tasks = model.tasks ++ [ Task model.nextId model.inputValue False ], inputValue = "", nextId = model.nextId + 1 }

        DeleteTask index ->
            { model | tasks = List.filter (\l -> l.id /= index) model.tasks }

        ToggleCheckbox index ->
            let
                toggle task =
                    if index == task.id then
                        { task | isComplete = not task.isComplete }

                    else
                        task
            in
            { model | tasks = List.map toggle model.tasks }

        ChangeInput value ->
            { model | inputValue = value }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "TODO List" ]
        , input [ placeholder "Next Task...", value model.inputValue, onInput ChangeInput ] []
        , button [ onClick AddTask ] [ text "Add Task" ]
        , table []
            (List.map (\l -> tr [] [ td [] [ text l.content ], td [] [ input [ type_ "checkbox", checked l.isComplete, onClick (ToggleCheckbox l.id) ] [] ], td [] [ button [ onClick (DeleteTask l.id) ] [ text "Delete Task" ] ] ]) model.tasks)
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
