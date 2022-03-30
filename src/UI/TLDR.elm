module UI.TLDR exposing (Corners, TLDR, corners, edges, tldr)


type alias TLDR =
    { top : Int
    , left : Int
    , bottom : Int
    , right : Int
    }


edges : TLDR
edges =
    { top = 0
    , left = 0
    , bottom = 0
    , right = 0
    }


tldr : Int -> TLDR
tldr i =
    TLDR i i i i


type alias Corners =
    { topLeft : Int
    , topRight : Int
    , bottomLeft : Int
    , bottomRight : Int
    }


corners : Corners
corners =
    { topLeft = 0
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }
