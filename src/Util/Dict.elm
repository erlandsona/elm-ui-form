module Util.Dict exposing (at)

import Accessors exposing (Relation)
import Dict exposing (Dict)


at :
    (comparable -> String)
    -> comparable
    -- -> Lens (Dict Int value) value value value
    -> Relation value value value
    -> Relation (Dict comparable value) value (Dict comparable value)
at toStr idx =
    let
        name : String
        name =
            "[" ++ toStr idx ++ "]"
    in
    Accessors.makeOneToN name (upsert idx) (upsert idx)


upsert : comparable -> (v -> v) -> Dict comparable v -> Dict comparable v
upsert idx fn =
    Dict.update idx (Maybe.map fn)
