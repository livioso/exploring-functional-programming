import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

type alias Model =
  { name: String
  , password: String
  , passwordAgain: String
  }

-- Update
model : Model
model =
  Model "" "" ""

-- Update
type Msg
  = Name String
  | Password String
  | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }
    Password password ->
      { model | password = password }
    PasswordAgain password ->
      { model | passwordAgain = password }

view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "name", onInput Name ] []
    , input [ type' "text", placeholder "password", onInput Password ] []
    , input [ type' "text", placeholder "Re-enter password", onInput PasswordAgain ] []
    , viewValidation model
    ]

viewValidation : Model -> Html Msg
viewValidation model =
  let 
    (color, message) =
      if model.password == model.passwordAgain then
        ("green", "ok")
      else
        ("red", "Password do not match")
  in
    div [ style [("color", color)] ] [ text message ]
