module Gen.Lens exposing (..)

import Accessors exposing (Lens, Relation, makeOneToOne)


altEmails : Lens { record | altEmails : altEmails } altEmails sub wrap
altEmails =
    makeOneToOne ".altEmails" .altEmails (\f r -> { r | altEmails = f r.altEmails })


id : Lens { record | id : id } id sub wrap
id =
    makeOneToOne ".id" .id (\f r -> { r | id = f r.id })


changePassword : Lens { record | changePassword : changePassword } changePassword sub wrap
changePassword =
    makeOneToOne
        ".changePassword"
        .changePassword
        (\f r -> { r | changePassword = f r.changePassword })


data : Lens { record | data : data } data sub wrap
data =
    makeOneToOne ".data" .data (\f r -> { r | data = f r.data })


email : Lens { record | email : email } email sub wrap
email =
    makeOneToOne ".email" .email (\f r -> { r | email = f r.email })


floatErrors : Lens { record | floatErrors : floatErrors } floatErrors sub wrap
floatErrors =
    makeOneToOne ".floatErrors" .floatErrors (\f r -> { r | floatErrors = f r.floatErrors })


name : Lens { record | name : name } name sub wrap
name =
    makeOneToOne ".name" .name (\f r -> { r | name = f r.name })


time : Lens { record | time : time } time sub wrap
time =
    makeOneToOne ".time" .time (\f r -> { r | time = f r.time })


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
