module Gen.Lens exposing (..)

import Accessors exposing (Lens, Relation, makeOneToOne)


age : Lens { record | age : age } t age b
age =
    makeOneToOne ".age" .age (\f r -> { r | age = f r.age })


changePassword : Lens { record | changePassword : changePassword } t changePassword b
changePassword =
    makeOneToOne
        ".changePassword"
        .changePassword
        (\f r -> { r | changePassword = f r.changePassword })


region : Lens { record | region : region } t region b
region =
    makeOneToOne ".region" .region (\f r -> { r | region = f r.region })


name : Lens { record | name : name } t name b
name =
    makeOneToOne ".name" .name (\f r -> { r | name = f r.name })


id : Lens { record | id : id } t id b
id =
    makeOneToOne ".id" .id (\f r -> { r | id = f r.id })


email : Lens { record | email : email } t email b
email =
    makeOneToOne ".email" .email (\f r -> { r | email = f r.email })


data : Lens { record | data : data } t data b
data =
    makeOneToOne ".data" .data (\f r -> { r | data = f r.data })


floatErrors : Lens { record | floatErrors : floatErrors } t floatErrors b
floatErrors =
    makeOneToOne ".floatErrors" .floatErrors (\f r -> { r | floatErrors = f r.floatErrors })


time : Lens { record | time : time } t time b
time =
    makeOneToOne ".time" .time (\f r -> { r | time = f r.time })


showErrors : Lens { record | showErrors : showErrors } t showErrors b
showErrors =
    makeOneToOne ".showErrors" .showErrors (\f r -> { r | showErrors = f r.showErrors })


parser : Lens { record | parser : parser } t parser b
parser =
    makeOneToOne ".parser" .parser (\f r -> { r | parser = f r.parser })


kind : Lens { record | kind : kind } t kind b
kind =
    makeOneToOne ".kind" .kind (\f r -> { r | kind = f r.kind })


form : Lens { record | form : form } t form b
form =
    makeOneToOne ".form" .form (\f r -> { r | form = f r.form })
