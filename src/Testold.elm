module Test exposing (..)

import Time
import Time.Extra


a = 1
b = "Hello"


---------------------------------------------------
-- Config

type alias Config_EventView =
    { nodeName : String
    , classes : List ( String, Bool )
    -- , children : List (Html InternalMsg.Msg)
    }


type alias Config_ViewConfig event =
    { toId : event -> String
    , title : event -> String
    , start : event -> Time.Posix
    , end : event -> Time.Posix
    , event : event -> Bool -> Config_EventView
    }

---------------------------------------------------
-- Calander

--/ configure view definition

type ViewConfig event
    = ViewConfig (Config_ViewConfig event)

type EventView
    = EventView Config_EventView

--/ configure a custom event view


eventView :
    { nodeName : String
    , classes : List ( String, Bool )
    -- , children : List (Html InternalMsg.Msg)
    }
    -> EventView

-- eventView { nodeName, classes, children } =
eventView { nodeName, classes  } =
    EventView
        { nodeName = nodeName
        , classes = classes
        -- , children = children
        }

{-| configure the view
-}
viewConfig :
    { toId : event -> String
    , title : event -> String
    , start : event -> Time.Posix
    , end : event -> Time.Posix
    , event : event -> Bool -> EventView
    }
    -> ViewConfig event

viewConfig { toId, title, start, end, event } =
   let
        extractEventView eventView2 =
            case eventView2 of
                EventView eventView3 ->
                    eventView3

        eventView_ id selected =
            extractEventView <| event id selected
    in
        ViewConfig
            { toId = toId
            , title = title
            , start = start
            , end = end
            , event = eventView_
            }
---------------------------------------------------
-- Basic

type alias Event =
    { id : String
    , title : String
    , start : Time.Posix
    , end : Time.Posix  
    }

viewConfig2 : ViewConfig Event
viewConfig2 =
    viewConfig
        { toId = .id
        , title = .title
        , start = .start
        , end = .end
        , event =
            \event isSelected ->
                eventView
                    { nodeName = "div"
                    , classes =
                        [ ( "elm-calendar--event-content", True )
                        , ( "elm-calendar--event-content--is-selected", isSelected )
                        ]
                    -- , children =
                    --     [ div []
                    --         [ text <| event.title ]
                    --     ]
                    }
        }

----------------------------------


eventOne =
    { id = "1", title = "GUSA1/ Friends", start = Time.Extra.partsToPosix Time.utc (Time.Extra.Parts  2020 Time.Oct 9 3 0 0 0), end = Time.Extra.partsToPosix Time.utc (Time.Extra.Parts  2020 Time.Oct 9 5 0 0 0) }


eventTwo =
    { id = "2", title = "GUSA2/ Friends", start = Time.Extra.partsToPosix Time.utc (Time.Extra.Parts  2020 Time.Oct 9 12 0 0 0), end = Time.Extra.partsToPosix Time.utc (Time.Extra.Parts  2020 Time.Oct 9 13 0 0 0) }


eventThree =
    { id = "3", title = "GUSA3/ Friends", start = Time.Extra.partsToPosix Time.utc (Time.Extra.Parts  2020 Time.Oct 9 17 0 0 0), end = Time.Extra.partsToPosix Time.utc (Time.Extra.Parts  2020 Time.Oct 9 20 0 0 0) }


eventFour =
    { id = "4", title = "GUSA4/ Friends", start = Time.Extra.partsToPosix Time.utc (Time.Extra.Parts  2020 Time.Oct 9 4 0 0 0), end = Time.Extra.partsToPosix Time.utc (Time.Extra.Parts  2020 Time.Oct 9 7 0 0 0) }

eventFive =
    { id = "5", title = "GUSA5/ Friends", start = Time.Extra.partsToPosix Time.utc (Time.Extra.Parts  2020 Time.Oct 1 4 0 0 0), end = Time.Extra.partsToPosix Time.utc (Time.Extra.Parts  2020 Time.Oct 1 7 0 0 0)  }

