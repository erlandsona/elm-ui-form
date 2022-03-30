module GenerateLens exposing (accessors)

import Elm.CodeGen as CodeGen
import NoMissingRecordFieldLens exposing (FieldLensGenerator)


accessors : FieldLensGenerator
accessors =
    { imports =
        [ CodeGen.importStmt [ "Accessors" ]
            Nothing
            (CodeGen.exposeExplicit
                [ CodeGen.funExpose "makeOneToOne"
                , CodeGen.typeOrAliasExpose "Relation"
                ]
                |> Just
            )
        ]
    , declaration =
        \{ fieldName } ->
            { documentation = Nothing
            , name = fieldName
            , annotation =
                CodeGen.typed "Lens"
                    [ CodeGen.extRecordAnn "record"
                        [ ( fieldName, CodeGen.typeVar fieldName ) ]
                    , CodeGen.typeVar fieldName
                    , CodeGen.typeVar "sub"
                    , CodeGen.typeVar "wrap"
                    ]
                    |> Just
            , implementation =
                let
                    { access, update } =
                        NoMissingRecordFieldLens.functionsForField fieldName
                in
                CodeGen.construct "makeOneToOne"
                    [ CodeGen.string (String.cons '.' fieldName)
                    , access
                    , update
                    ]
            }
    }
