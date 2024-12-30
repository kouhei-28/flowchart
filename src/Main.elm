module Main exposing (..)

import Browser
import Html exposing (Html, div, node, text)
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse as Mouse
import Svg exposing (path, svg)
import Svg.Attributes



-- Type


type alias Position =
    { x : Float, y : Float }


type alias Node =
    { id : String, position : Position }


type alias Connector =
    { node : Node
    , leftAndRight : LeftAndRight
    }


type alias Edge =
    { edgeId : String
    , fromPosition : Position
    , toPosition : Position
    , fromConnector : Connector
    , toConnector : Maybe Connector
    }


type LeftAndRight
    = Left
    | Right


type Status
    = Grabbed String
    | Connecting String
    | Default



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { status : Status
    , nodes : List Node
    , edges : List Edge
    }


init : Model
init =
    Model Default [ Node "node1" (Position 100 100), Node "node2" (Position 400 100) ] []



-- UPDATE


type Msg
    = Grab String
    | Move ( Float, Float )
    | Release
    | SelectConnector Connector


updateGrabbedNodePosition : String -> Position -> Node -> Node
updateGrabbedNodePosition nodeId position node =
    if node.id == nodeId then
        { node | position = position }

    else
        node


updateConnectingEdge : String -> Position -> Maybe Connector -> Edge -> Edge
updateConnectingEdge edgeId position toConnector edge =
    if edge.edgeId == edgeId then
        { edge | toPosition = position, toConnector = toConnector }

    else
        edge


calculateConnectorPosition : Position -> LeftAndRight -> Position
calculateConnectorPosition nodePosition leftAndRight =
    case leftAndRight of
        Left ->
            Position (nodePosition.x + 6) (nodePosition.y + 29)

        Right ->
            Position (nodePosition.x + 152) (nodePosition.y + 29)


calculateEdgePositionWhenNodeMove : String -> Position -> Edge -> Edge
calculateEdgePositionWhenNodeMove nodeId newNodePosition edge =
    if edge.fromConnector.node.id == nodeId then
        { edge | fromPosition = calculateConnectorPosition newNodePosition edge.fromConnector.leftAndRight }

    else
        case edge.toConnector of
            Just toConnector ->
                if toConnector.node.id == nodeId then
                    { edge | toPosition = calculateConnectorPosition newNodePosition toConnector.leftAndRight }

                else
                    edge

            Nothing ->
                edge


update : Msg -> Model -> Model
update msg model =
    case msg of
        Grab nodeId ->
            case model.status of
                Default ->
                    { model | status = Grabbed nodeId }

                Grabbed _ ->
                    model

                Connecting _ ->
                    model

        Move ( x, y ) ->
            case model.status of
                Grabbed nodeId ->
                    let
                        updatedNodes =
                            List.map
                                (updateGrabbedNodePosition nodeId (Position (x - 79) (y - 29)))
                                model.nodes

                        updateEdges =
                            List.map (calculateEdgePositionWhenNodeMove nodeId (Position (x - 79) (y - 29))) model.edges
                    in
                    { model | nodes = updatedNodes, edges = updateEdges }

                Connecting edgeId ->
                    let
                        updateEdges =
                            List.map (updateConnectingEdge edgeId (Position x y) Nothing) model.edges
                    in
                    { model | edges = updateEdges }

                Default ->
                    model

        Release ->
            case model.status of
                Grabbed _ ->
                    { model | status = Default }

                Connecting _ ->
                    model

                Default ->
                    model

        SelectConnector connector ->
            case model.status of
                Default ->
                    let
                        edgeId =
                            "edge" ++ String.fromInt (List.length model.edges)

                        updateEdges =
                            Edge edgeId (calculateConnectorPosition connector.node.position connector.leftAndRight) (calculateConnectorPosition connector.node.position connector.leftAndRight) connector Nothing :: model.edges
                    in
                    { model | status = Connecting edgeId, edges = updateEdges }

                Connecting edgeId ->
                    let
                        toPosition =
                            calculateConnectorPosition connector.node.position connector.leftAndRight

                        updateEdges =
                            List.map (updateConnectingEdge edgeId toPosition (Just connector)) model.edges
                    in
                    { model | status = Default, edges = updateEdges }

                Grabbed _ ->
                    model



-- VIEW


view : Model -> Html Msg
view model =
    board model.nodes model.edges


board : List Node -> List Edge -> Html Msg
board nodes edges =
    div
        [ style "position" "relative"
        , style "width" "100vw"
        , style "height" "100vh"
        , Mouse.onUp (\_ -> Release)
        , Mouse.onMove (\event -> Move event.clientPos)
        , Mouse.onLeave (\_ -> Release)
        ]
        (List.append (List.map nodeComponent nodes) (List.map edgeComponent edges))


nodeComponent : Node -> Html Msg
nodeComponent node =
    div
        [ style "position" "absolute"
        , style "transform" ("translate(" ++ String.fromFloat node.position.x ++ "px," ++ String.fromFloat node.position.y ++ "px)")
        ]
        [ div
            [ style "display" "flex"
            , style "align-items" "center"
            ]
            [ div
                [ style "width" "10px"
                , style "height" "10px"
                , style "border" "solid 1px"
                , style "border-radius" "50%"
                , style "cursor" "pointer"
                , Mouse.onClick (\_ -> SelectConnector (Connector node Left))
                ]
                []
            , div
                [ style "border" "solid 1px"
                , style "width" "100px"
                , style "height" "40px"
                , style "border-radius" "6px"
                , style "padding" "8px 16px"
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "cursor" "grab"
                , Mouse.onDown (\_ -> Grab node.id)
                ]
                [ text node.id ]
            , div
                [ style "width" "10px"
                , style "height" "10px"
                , style "border" "solid 1px"
                , style "border-radius" "50%"
                , style "cursor" "pointer"
                , Mouse.onClick (\_ -> SelectConnector (Connector node Right))
                ]
                []
            ]
        ]


edgeComponent : Edge -> Html Msg
edgeComponent edge =
    let
        c1x = edge.fromPosition.x + abs (edge.toPosition.x - edge.fromPosition.x)
        c1y = edge.fromPosition.y
        c2x = edge.toPosition.x - abs (edge.toPosition.x - edge.fromPosition.x)
        c2y = edge.toPosition.y
    in
    div
        [ style "position" "absolute"
        , style "pointer-events" "none"
        ]
        [ svg
            [ style "width" "100vw"
            , style "height" "100vh"
            ]
            [ path
                [ Svg.Attributes.d ("M " ++ String.fromFloat edge.fromPosition.x ++ " " ++ String.fromFloat edge.fromPosition.y ++ " C " ++ String.fromFloat c1x ++ " " ++ String.fromFloat c1y ++ " " ++ String.fromFloat c2x ++ " " ++ String.fromFloat c2y ++ " " ++ String.fromFloat edge.toPosition.x ++ " " ++ String.fromFloat edge.toPosition.y)
                , Svg.Attributes.stroke "black"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.fill "none"
                ]
                []
            ]
        ]
