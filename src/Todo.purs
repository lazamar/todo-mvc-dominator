module Todo where

{-| TodoMVC implemented in Dominator, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML

This clean division of concerns is a core part of Dominator. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>
-}

import Prelude hiding (div,id)

import Dominator.Html
import Dominator.Html.Attributes
import Dominator.Html.Events
import Dominator.Cmd (Cmds)
import Dominator.Decode (Decoder, succeed, fail)
import Dominator.Html.Keyed as Keyed
import Dominator.Html.Lazy (lazy, lazy2)
import Dominator.Operators ((|>), (<|), (!))

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Array.ST (unsafeFreeze)
import Data.Either (Either(Left, Right))
import Data.Foldable as Foldable
import Data.Foreign (Foreign, readNullOrUndefined, unsafeFromForeign)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>), mempty)
import Data.String as String
import Data.Tuple (Tuple(Tuple))
import Data.Generic (class Generic)
import Data.Foreign.Class (class Encode, class Decode, encode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecodeJSON, genericEncodeJSON)

embed :: HtmlElement -> Foreign -> Eff Effs Unit
embed el flags = program (Just el)
    { init : init (parseFlags flags)
    , update : updateWithStorage
    , view : view
    }

foreign import data LocalStorage :: Effect

foreign import setStorage :: forall msg a. Model -> Eff (localStorage :: LocalStorage | a) msg

foreign import focusElement :: forall msg a. String -> Eff (dom :: DOM | a) msg

type Effs = (dom :: DOM, localStorage :: LocalStorage)

{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage :: Msg -> Model -> Tuple Model (Cmds Effs Msg)
updateWithStorage msg model =
    let
        Tuple newModel cmds  =
            update msg model
    in
        newModel
        ! cmds <> [ liftEff $ setStorage newModel ]
        



-- MODEL


-- The full application state of our todo app.
type Model =
    { entries :: List Entry
    , field :: String
    , uid :: Int
    , visibility :: String
    }


newtype Entry = Entry
    { description :: String
    , completed :: Boolean
    , editing :: Boolean
    , id :: Int
    }

derive instance genericEntry :: Generic Entry

emptyModel :: Model
emptyModel =
    { entries : mempty
    , visibility : "All"
    , field : ""
    , uid : 0
    }


newEntry :: String -> Int -> Entry
newEntry desc id = Entry
    { description : desc
    , completed : false
    , editing : false
    , id : id
    }


parseFlags :: Foreign -> Maybe Model 
parseFlags v =
    let
        parsed = v
            |> readNullOrUndefined
            |> map (map unsafeFromForeign)
            |> runExcept
    in
        case parsed of
            Right m -> m
            Left _ -> Nothing


init :: Maybe Model -> Tuple Model (Cmds Effs Msg)
init maybeModel = 
    case maybeModel of
        Nothing -> emptyModel ! []
        Just model -> model ! []



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
data Msg
    = NoOp
    | UpdateField String
    | EditingEntry Int Boolean
    | UpdateEntry Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Boolean
    | CheckAll Boolean
    | ChangeVisibility String



-- How we update our Model on a given Msg?
update :: Msg -> Model -> Tuple Model (Cmds Effs Msg)
update msg model = 
    case msg of
        NoOp ->
            model ! []

        Add ->
            model
                { uid = model.uid + 1
                , field = ""
                , entries =
                    if String.null model.field then
                        model.entries
                    else
                        -- TODO: Set correct order here
                        (newEntry model.field model.uid) : model.entries
                }
                ! []

        UpdateField str ->
            model { field = str } ! []

        EditingEntry id isEditing ->
            let
                updateEntry (Entry t) = Entry $
                    if t.id == id then
                        t { editing = isEditing }
                    else
                        t

                focus =
                    focusElement ("todo-" <> show id)
            in
                model { entries = map updateEntry model.entries }
                    ! [ liftEff focus ]

        UpdateEntry id task ->
            let
                updateEntry (Entry t) = Entry $
                    if t.id == id then
                        t { description = task }
                    else
                        t
            in
                model { entries = map updateEntry model.entries }
                    ! []

        Delete id ->
            model { entries = List.filter (\t -> entryId t /= id) model.entries }
                ! []

        DeleteComplete ->
            model { entries = List.filter (not <<< isCompleted) model.entries }
                ! []

        Check id isCompleted ->
            let
                updateEntry (Entry t) = Entry $
                    if t.id == id then
                        t { completed = isCompleted }
                    else
                        t
            in
                model { entries = map updateEntry model.entries }
                    ! []

        CheckAll isCompleted ->
            let
                updateEntry (Entry t) =
                    Entry $ t { completed = isCompleted }
            in
                model { entries = map updateEntry model.entries }
                    ! []

        ChangeVisibility visibility ->
            model { visibility = visibility } ! []

-- VIEW

isCompleted :: Entry -> Boolean
isCompleted (Entry todo) = todo.completed

entryId :: Entry -> Int
entryId (Entry todo) = todo.id

view :: Model -> Html Msg
view model =
    div [ class_ "todomvc-wrapper"
        , style [ ( "visibility" ! "hidden" ) ]
        ]
        [ section
            [ class_ "todoapp" ]
            [ lazy viewInput model.field
            , lazy2 viewEntries model.visibility model.entries
            , lazy2 viewControls model.visibility model.entries
            ]
        , infoFooter
        ]


viewInput :: String -> Html Msg
viewInput task =
    header
        [ class_ "header" ]
        [ h1 [] [ text "todos" ]
        , input
            [ class_ "new-todo"
            , placeholder "What needs to be done?"
            , autofocus true
            , value task
            , name "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]


onEnter :: Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                succeed msg
            else
                fail "not ENTER"
    in
        on "keydown" $ \v -> keyCode v >>= isEnter



-- -- -- VIEW ALL ENTRIES

viewEntries :: String -> List Entry -> Html Msg
viewEntries visibility entries =
    let
        isVisible (Entry todo) =
            case visibility of
                "Completed" ->
                    todo.completed

                "Active" ->
                    not todo.completed

                _ ->
                    true

        allCompleted =
            Foldable.all isCompleted entries

        cssVisibility =
            if List.null entries then
                "hidden"
            else
                "visible"
    in
        section
            [ class_ "main"
            , style [ ( "visibility" ! cssVisibility ) ]
            ]
            [ input
                [ class_ "toggle-all"
                , type_ "checkbox"
                , name "toggle"
                , checked allCompleted
                , onClick (CheckAll (not allCompleted))
                ]
                []
            , label
                [ for "toggle-all" ]
                [ text "Mark all as complete" ]
            , Keyed.ul [ class_ "todo-list" ] <|
                Array.fromFoldable (map viewKeyedEntry (List.filter isVisible entries))
            ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntry :: Entry -> Tuple String (Html Msg) 
viewKeyedEntry entry =
    ( show (entryId entry) ! lazy viewEntry entry )


viewEntry :: Entry -> Html Msg
viewEntry (Entry todo) =
    li
        [ classList [ ( "completed" ! todo.completed ), ( "editing" ! todo.editing ) ] ]
        [ div
            [ class_ "view" ]
            [ input
                [ class_ "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onClick (Check todo.id (not todo.completed))
                ]
                []
            , label
                [ onDoubleClick (EditingEntry todo.id true) ]
                [ text todo.description ]
            , button
                [ class_ "destroy"
                , onClick (Delete todo.id)
                ]
                []
            ]
        , input
            [ class_ "edit"
            , value todo.description
            , name "title"
            , id ("todo-" <> show todo.id)
            , onInput (UpdateEntry todo.id)
            , onBlur (EditingEntry todo.id false)
            , onEnter (EditingEntry todo.id false)
            ]
            []
        ]



-- -- VIEW CONTROLS AND FOOTER


viewControls :: String -> List Entry -> Html Msg
viewControls visibility entries =
    let
        entriesCompleted =
            List.length (List.filter isCompleted entries)

        entriesLeft =
            List.length entries - entriesCompleted
    in
        footer
            [ class_ "footer"
            , hidden (List.null entries)
            ]
            [ lazy viewControlsCount entriesLeft
            , lazy viewControlsFilters visibility
            , lazy viewControlsClear entriesCompleted
            ]


viewControlsCount :: Int -> Html Msg
viewControlsCount entriesLeft =
    let
        item_ =
            if entriesLeft == 1 then
                " item"
            else
                " items"
    in
        span
            [ class_ "todo-count" ]
            [ strong [] [ text (show entriesLeft) ]
            , text (item_ <> " left")
            ]


viewControlsFilters :: String -> Html Msg
viewControlsFilters visibility =
    ul
        [ class_ "filters" ]
        [ visibilitySwap "#/" "All" visibility
        , text " "
        , visibilitySwap "#/active" "Active" visibility
        , text " "
        , visibilitySwap "#/completed" "Completed" visibility
        ]


visibilitySwap :: String -> String -> String -> Html Msg
visibilitySwap uri visibility actualVisibility =
    li
        [ onClick (ChangeVisibility visibility) ]
        [ a [ href uri, classList [ ( "selected" ! visibility == actualVisibility ) ] ]
            [ text visibility ]
        ]


viewControlsClear :: Int -> Html Msg
viewControlsClear entriesCompleted =
    button
        [ class_ "clear-completed"
        , hidden (entriesCompleted == 0)
        , onClick DeleteComplete
        ]
        [ text ("Clear completed (" <> show entriesCompleted <> ")")
        ]


infoFooter :: forall msg. Html msg
infoFooter =
    footer [ class_ "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Code adapted by " 
            , a [ href "https://github.com/lazamar" ] [ text "Marcelo Lazaroni" ] 
            , text " showing Dominator's virtual DOM in Purescript. " 
            ]
        , p []
            [ text "Originally written by " 
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ] 
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]