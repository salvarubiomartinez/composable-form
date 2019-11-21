module Image exposing (Model, Msg, getImage, getImageUrl, imageRequestedMsg, init, removeImageMsg, update, view)

import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events exposing (onClick)
import Task


type Model
    = Model
        { image : Maybe File
        , imageUrl : Maybe String
        }


init : Maybe String -> Model
init imageUrl =
    Model
        { image = Nothing
        , imageUrl = imageUrl
        }


getImage : Model -> Maybe File
getImage (Model model) =
    model.image


getImageUrl : Model -> Maybe String
getImageUrl (Model model) =
    model.imageUrl


imageRequestedMsg : Msg
imageRequestedMsg =
    ImageRequested


removeImageMsg : Msg
removeImageMsg =
    RemoveImage


type Msg
    = ImageSelected File
    | ImageLoaded String
    | ImageRequested
    | RemoveImage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        ImageSelected file ->
            ( Model { model | image = Just file }
            , Task.perform ImageLoaded (File.toUrl file)
            )

        ImageLoaded content ->
            ( Model { model | imageUrl = Just content }, Cmd.none )

        RemoveImage ->
            ( Model
                { model
                    | image = Nothing
                    , imageUrl = Nothing
                }
            , Cmd.none
            )

        ImageRequested ->
            ( Model model
            , Select.file [ "image/jpg", "image/jpeg", "image/png" ] ImageSelected
            )


view : Model -> Html Msg
view (Model model) =
    Html.div []
        [ Html.button [ onClick ImageRequested ] [ Html.text "add image" ]
        , Html.button [ onClick RemoveImage ] [ Html.text "delete image" ]
        , Html.img [ Attributes.src <| Maybe.withDefault "" model.imageUrl ] []
        ]
