module Main exposing (main)

import Browser
import Html exposing (div, h1, h2, button, li, ul, select, option, text)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import Svg exposing (svg, line, circle, g, text_)
import Svg.Attributes as SvgAttr
import Task
import Process
import Debug
import Basics exposing (ceiling)

-- MODEL
type alias DijkstraStep =
    { current : Int
    , visited : List Int
    , distances : List (Int, Int)
    }

type alias Model =
    { graph : List (Int, List (Int, Int))
    , distances : List (Int, Int)
    , error : Maybe String
    , startNode : Int
    , endNode : Int
    , steps : List DijkstraStep
    , currentStep : Int
    , playing : Bool
    , shortestPath : List Int
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( { graph =
            [ (1, [ (2, 7), (3, 9), (6, 14) ])
            , (2, [ (1, 7), (3, 10), (4, 15) ])
            , (3, [ (1, 9), (2, 10), (4, 11), (6, 2) ])
            , (4, [ (2, 15), (3, 11), (5, 6) ])
            , (5, [ (4, 6), (6, 9) ])
            , (6, [ (1, 14), (3, 2), (5, 9) ])
            ]
      , distances = []
      , error = Nothing
      , startNode = 1
      , endNode = 5
      , steps = []
      , currentStep = 0
      , playing = False
      , shortestPath = []
      }
    , Cmd.none
    )

-- UPDATE
type Msg
    = CalculatePath
    | GotResult (Result Http.Error (List (Int, Int), List DijkstraStep, List Int))
    | SetStartNode String
    | SetEndNode String
    | StepForward
    | StepBackward
    | ResetSteps
    | Play
    | Pause
    | NextStepTick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetStartNode s ->
            case String.toInt s of
                Just n -> ({ model | startNode = n }, Cmd.none)
                Nothing -> (model, Cmd.none)
        SetEndNode s ->
            case String.toInt s of
                Just n -> ({ model | endNode = n }, Cmd.none)
                Nothing -> (model, Cmd.none)
        CalculatePath ->
            ( { model | playing = False, currentStep = 0, shortestPath = [] }
            , Debug.log "Sending request with startNode" model.startNode
                |> (\_ -> Debug.log "Sending request with endNode" model.endNode)
                |> (\_ -> Http.post
                { url = "http://localhost:3000/api/shortest-path"
                , body = Http.jsonBody (encodeGraph model.graph model.startNode model.endNode)
                , expect = Http.expectJson GotResult resultDecoder
                })
            )
        GotResult result ->
            case result of
                Ok (distances, steps, shortestPath) ->
                    ( { model | distances = distances, error = Nothing, steps = steps, currentStep = 0, playing = False, shortestPath = shortestPath }
                    , Cmd.none
                    )
                Err _ ->
                    ( { model | error = Just "Failed to calculate path", steps = [], currentStep = 0, playing = False, shortestPath = [] }
                    , Cmd.none
                    )
        StepForward ->
            if model.currentStep < List.length model.steps - 1 then
                ( { model | currentStep = model.currentStep + 1 }, Cmd.none )
            else
                ( { model | playing = False }, Cmd.none )
        StepBackward ->
            if model.currentStep > 0 then
                ( { model | currentStep = model.currentStep - 1 }, Cmd.none )
            else
                (model, Cmd.none)
        ResetSteps ->
            ( { model | currentStep = 0, playing = False }, Cmd.none )
        Play ->
            if model.currentStep < List.length model.steps - 1 then
                ( { model | playing = True }, Task.perform (\_ -> NextStepTick) (Process.sleep 500) )
            else
                (model, Cmd.none)
        Pause ->
            ( { model | playing = False }, Cmd.none )
        NextStepTick ->
            if model.playing && model.currentStep < List.length model.steps - 1 then
                ( { model | currentStep = model.currentStep + 1 }
                , Task.perform (\_ -> NextStepTick) (Process.sleep 500)
                )
            else
                ( { model | playing = False }, Cmd.none )

-- VIEW
view : Model -> Html.Html Msg
view model =
    let
        currentStepData =
            if List.isEmpty model.steps then
                Nothing
            else
                List.drop model.currentStep model.steps |> List.head

        animationFinished = model.currentStep == List.length model.steps - 1
    in
    div [ HtmlAttr.class "container" ]
        [ h1 [] [ Html.text "Dijkstra's Algorithm Visualization" ]
        , div []
            [ text "Start Node: "
            , select [ onInput SetStartNode ]
                (List.map (\n -> option [ HtmlAttr.value (String.fromInt n), HtmlAttr.selected (n == model.startNode) ] [ text (String.fromInt n) ]) (List.map Tuple.first model.graph))
            , text " End Node: "
            , select [ onInput SetEndNode ]
                (List.map (\n -> option [ HtmlAttr.value (String.fromInt n), HtmlAttr.selected (n == model.endNode) ] [ text (String.fromInt n) ]) (List.map Tuple.first model.graph))
            ]
        , div [ HtmlAttr.class "graph-container" ]
            [ svg
                [ SvgAttr.width "600"
                , SvgAttr.height "400"
                , SvgAttr.viewBox "0 0 600 400"
                ]
                (drawGraph model currentStepData animationFinished)
            ]
        , div []
            [ button [ onClick CalculatePath ] [ Html.text "Calculate Shortest Path" ]
            , button [ onClick StepBackward, HtmlAttr.disabled (model.currentStep == 0) ] [ Html.text "Step Back" ]
            , button [ onClick StepForward, HtmlAttr.disabled (model.currentStep >= List.length model.steps - 1) ] [ Html.text "Step Forward" ]
            , button [ onClick ResetSteps ] [ Html.text "Reset" ]
            , if model.playing then
                button [ onClick Pause ] [ Html.text "Pause" ]
              else
                button [ onClick Play, HtmlAttr.disabled (model.currentStep >= List.length model.steps - 1) ] [ Html.text "Play" ]
            ]
        , div [ HtmlAttr.class "results" ]
            [ h2 [] [ Html.text "Distances:" ]
            , ul [] (List.map viewDistance (Maybe.withDefault [] (Maybe.map .distances currentStepData)))
            ]
        , viewError model.error
        ]

drawGraph : Model -> Maybe DijkstraStep -> Bool -> List (Svg.Svg msg)
drawGraph model maybeStep animationFinished =
    let
        nodePositions =
            [ (1, (100, 100))
            , (2, (300, 100))
            , (3, (200, 200))
            , (4, (400, 200))
            , (5, (500, 300))
            , (6, (200, 300))
            ]
        visitedNodes =
            case maybeStep of
                Just step -> step.visited
                Nothing -> []
        currentNode =
            case maybeStep of
                Just step -> step.current
                Nothing -> -1
    in
    List.concat
        [ drawEdges model.graph nodePositions model.shortestPath animationFinished
        , drawNodes nodePositions visitedNodes currentNode model.shortestPath animationFinished
        ]

drawEdges : List (Int, List (Int, Int)) -> List (Int, (Float, Float)) -> List Int -> Bool -> List (Svg.Svg msg)
drawEdges graph nodePositions shortestPath animationFinished =
    let
        getPosition node =
            Maybe.withDefault (0, (0.0, 0.0)) (List.head (List.filter (\(n, _) -> n == node) nodePositions))
            |> Tuple.second

        isPathEdge from to =
            let
                pathPairs = List.map2 Tuple.pair shortestPath (List.drop 1 shortestPath)
            in
            List.member (from, to) pathPairs || List.member (to, from) pathPairs
    in
    List.concatMap
        (\(from, edges) ->
            List.map
                (\(to, weight) ->
                    let
                        (x1, y1) = getPosition from
                        (x2, y2) = getPosition to
                        pathEdge = animationFinished && isPathEdge from to
                        strokeColor = if pathEdge then "green" else "black"
                        strokeWidth = if pathEdge then "4" else "1"
                    in
                    g []
                        [ line
                            [ SvgAttr.x1 (String.fromFloat x1)
                            , SvgAttr.y1 (String.fromFloat y1)
                            , SvgAttr.x2 (String.fromFloat x2)
                            , SvgAttr.y2 (String.fromFloat y2)
                            , SvgAttr.stroke strokeColor
                            , SvgAttr.strokeWidth strokeWidth
                            ]
                            []
                        , text_
                            [ SvgAttr.x (String.fromFloat ((x1 + x2) / 2))
                            , SvgAttr.y (String.fromFloat ((y1 + y2) / 2))
                            , SvgAttr.textAnchor "middle"
                            , SvgAttr.fill strokeColor
                            ]
                            [ Svg.text (String.fromInt weight) ]
                        ]
                )
                edges
        )
        graph

drawNodes : List (Int, (Float, Float)) -> List Int -> Int -> List Int -> Bool -> List (Svg.Svg msg)
drawNodes nodePositions visitedNodes currentNode shortestPath animationFinished =
    List.map
        (\(node, (x, y)) ->
            let
                isVisited = List.member node visitedNodes
                isCurrent = node == currentNode
                isOnPath = List.member node shortestPath
                
                -- Determine colors based on animation state
                fillColor =
                    if isCurrent then
                        "#ffeb3b" -- Yellow for current node
                    else if isVisited then
                        "#90caf9" -- Light blue for visited nodes
                    else
                        "white"    -- Default white

                strokeColor =
                    if animationFinished && isOnPath then
                        "green"    -- Green for shortest path nodes (only when animation finished)
                    else if isCurrent then
                        "#fbc02d" -- Darker yellow for current node stroke
                    else if isVisited then
                        "#1976d2" -- Darker blue for visited node stroke
                    else
                        "black"    -- Default black

                strokeWidth =
                    if animationFinished && isOnPath then
                        "4"
                    else if isCurrent then
                        "3"
                    else
                        "1"

                textColor =
                    if animationFinished && isOnPath then
                        "white"
                    else
                        "black"

            in
            g []
                [ circle
                    [ SvgAttr.cx (String.fromFloat x)
                    , SvgAttr.cy (String.fromFloat y)
                    , SvgAttr.r "20"
                    , SvgAttr.fill fillColor
                    , SvgAttr.stroke strokeColor
                    , SvgAttr.strokeWidth strokeWidth
                    ]
                    []
                , text_
                    [ SvgAttr.x (String.fromFloat x)
                    , SvgAttr.y (String.fromFloat y)
                    , SvgAttr.textAnchor "middle"
                    , SvgAttr.dominantBaseline "middle"
                    , SvgAttr.fill textColor
                    ]
                    [ Svg.text (String.fromInt node) ]
                ]
        )
        nodePositions

viewDistance : (Int, Int) -> Html.Html msg
viewDistance (node, distance) =
    li [ HtmlAttr.style "background-color" "#eee", HtmlAttr.style "margin" "4px 0", HtmlAttr.style "padding" "8px", HtmlAttr.style "border-radius" "4px" ]
        [ Html.text ("Node " ++ String.fromInt node ++ ": " ++ String.fromInt distance) ]

viewError : Maybe String -> Html.Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            div [ HtmlAttr.class "error" ] [ Html.text error ]
        Nothing ->
            Html.text ""

-- ENCODERS/DECODERS
encodeGraph : List (Int, List (Int, Int)) -> Int -> Int -> E.Value
encodeGraph graph startNode endNode =
    E.object
        [ ( "graph"
          , E.list
                (\(node, edges) ->
                    E.object
                        [ ( "node", E.int node )
                        , ( "edges"
                          , E.list
                                (\(to, weight) ->
                                    E.object
                                        [ ( "to", E.int to )
                                        , ( "weight", E.int weight )
                                        ]
                                )
                                edges
                          )
                        ]
                )
                graph
          )
        , ( "start", E.int startNode )
        , ( "end", E.int endNode )
        ]

-- DECODER FOR BACKEND RESPONSE
resultDecoder : D.Decoder (List (Int, Int), List DijkstraStep, List Int)
resultDecoder =
    D.map3 (\a b c -> (a, b, c))
        (D.field "distances" (D.list (D.map2 Tuple.pair (D.index 0 D.int) (D.index 1 D.int))))
        (D.field "steps" (D.list dijkstraStepDecoder))
        (D.field "shortestPath" (D.list D.int))

dijkstraStepDecoder : D.Decoder DijkstraStep
dijkstraStepDecoder =
    D.map3 DijkstraStep
        (D.field "current" D.int)
        (D.field "visited" (D.list D.int))
        (D.field "distances" (D.list (D.map2 Tuple.pair (D.index 0 D.int) (D.index 1 D.int))))

-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        } 