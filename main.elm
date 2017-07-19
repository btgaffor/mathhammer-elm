import Html exposing (Html, program)

import Model exposing (..)
import Update exposing (..)
import View exposing (..)

-- main : Program Never Model Msg
-- main =
--   Html.beginnerProgram { model = model, view = view, update = update }

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
