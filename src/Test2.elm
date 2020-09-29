module Test exposing (..)

import Time
import Time.Extra


a = 1
b = "Hello"


---------------------------------------------------
-- Config
---------------------------------------------------

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
---------------------------------------------------

--/ configure view definition

type ViewConfig event
    = ViewConfig (Config_ViewConfig event)

-- event view type

type EventView
    = EventView Config_EventView

--/ configure a custom event view

eventView :
    { nodeName : String
    , classes : List ( String, Bool )
    -- , children : List (Html InternalMsg.Msg)
    }
    -> EventView

eventView { nodeName, classes  } =
    EventView
        { nodeName = nodeName
        , classes = classes
        -- , children = children
        }

-- configure the view

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
---------------------------------------------------

type alias Event =
    { id : String
    , title : String
    , start : Time.Posix
    , end : Time.Posix  
    }

config : ViewConfig Event
config =
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


-- eventOne :Event
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

events =
    [ eventOne
    , eventTwo
    , eventThree
    , eventFour
    , eventFive
    ]


---------------------------------------------------
-- get Op Test
---------------------------------------------------

getstart:  ViewConfig event -> event -> Time.Posix
getstart (ViewConfig config_) event =
     config_.start event

gettitle:  ViewConfig event -> event -> String
gettitle (ViewConfig config_) event =
     config_.title event

-------------------------------------------
--   > modlist config events
-------------------------------------------


modlist:  ViewConfig event -> List event -> List event
modlist (ViewConfig config_) events_ =
     -- events_
     List.map (makeslit config_)  events_

makeslit: Config_ViewConfig event -> event -> event 
makeslit  config4 event =
       event

---------------------------------------------------
{-
   # elm repl

   > import Test exposing (..)

   > getstart config eventOne 
   Posix 1602212400000 : Time.Posix

   > gettitle config eventOne
   "GUSA1/ Friends" : String

   > modlist config events

-}
