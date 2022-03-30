module Gen.Lens exposing (..)

import Accessors exposing (Relation, makeOneToOne)


type alias Lens super sub reachable wrap =
    Relation sub reachable wrap
    -> Relation super reachable wrap


type alias Property super sub =
    -- Relation sub sub sub
    -- -> Relation super sub sub
    Lens super sub sub sub


type alias Prism super ctor sub wrap =
    Relation super sub wrap
    -> Relation ctor sub (Maybe wrap)


floatErrors : Lens { record | floatErrors : floatErrors } floatErrors sub wrap
floatErrors =
    makeOneToOne ".floatErrors" .floatErrors (\f r -> { r | floatErrors = f r.floatErrors })


showErrors : Lens { record | showErrors : showErrors } showErrors sub wrap
showErrors =
    makeOneToOne ".showErrors" .showErrors (\f r -> { r | showErrors = f r.showErrors })


parser : Lens { record | parser : parser } parser sub wrap
parser =
    makeOneToOne ".parser" .parser (\f r -> { r | parser = f r.parser })


kind : Lens { record | kind : kind } kind sub wrap
kind =
    makeOneToOne ".kind" .kind (\f r -> { r | kind = f r.kind })


form : Lens { record | form : form } form sub wrap
form =
    makeOneToOne ".form" .form (\f r -> { r | form = f r.form })
