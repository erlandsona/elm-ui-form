module Gen.Lens exposing (..)

import Accessors exposing (Lens, makeOneToOne_)


address : Lens { record | address : attribute } transformed attribute built
address =
    makeOneToOne_ ".address" .address (\f r -> { r | address = f r.address })


zip : Lens { record | zip : attribute } transformed attribute built
zip =
    makeOneToOne_ ".zip" .zip (\f r -> { r | zip = f r.zip })


street : Lens { record | street : attribute } transformed attribute built
street =
    makeOneToOne_ ".street" .street (\f r -> { r | street = f r.street })


age : Lens { record | age : attribute } transformed attribute built
age =
    makeOneToOne_ ".age" .age (\f r -> { r | age = f r.age })


id : Lens { record | id : attribute } transformed attribute built
id =
    makeOneToOne_ ".id" .id (\f r -> { r | id = f r.id })


data : Lens { record | data : attribute } transformed attribute built
data =
    makeOneToOne_ ".data" .data (\f r -> { r | data = f r.data })


changePassword : Lens { record | changePassword : attribute } transformed attribute built
changePassword =
    makeOneToOne_
        ".changePassword"
        .changePassword
        (\f r -> { r | changePassword = f r.changePassword })


time : Lens { record | time : attribute } transformed attribute built
time =
    makeOneToOne_ ".time" .time (\f r -> { r | time = f r.time })


showErrors : Lens { record | showErrors : attribute } transformed attribute built
showErrors =
    makeOneToOne_ ".showErrors" .showErrors (\f r -> { r | showErrors = f r.showErrors })


parser : Lens { record | parser : attribute } transformed attribute built
parser =
    makeOneToOne_ ".parser" .parser (\f r -> { r | parser = f r.parser })


kind : Lens { record | kind : attribute } transformed attribute built
kind =
    makeOneToOne_ ".kind" .kind (\f r -> { r | kind = f r.kind })


form : Lens { record | form : attribute } transformed attribute built
form =
    makeOneToOne_ ".form" .form (\f r -> { r | form = f r.form })


floatErrors : Lens { record | floatErrors : attribute } transformed attribute built
floatErrors =
    makeOneToOne_ ".floatErrors" .floatErrors (\f r -> { r | floatErrors = f r.floatErrors })


stuff : Lens { record | stuff : attribute } transformed attribute built
stuff =
    makeOneToOne_ ".stuff" .stuff (\f r -> { r | stuff = f r.stuff })


region : Lens { record | region : attribute } transformed attribute built
region =
    makeOneToOne_ ".region" .region (\f r -> { r | region = f r.region })


name : Lens { record | name : attribute } transformed attribute built
name =
    makeOneToOne_ ".name" .name (\f r -> { r | name = f r.name })


email : Lens { record | email : attribute } transformed attribute built
email =
    makeOneToOne_ ".email" .email (\f r -> { r | email = f r.email })
