module GenerateLens exposing (accessors)

import Elm.CodeGen as CodeGen
import NoMissingRecordFieldLens exposing (FieldLensGenerator)


accessors : FieldLensGenerator
accessors =
    { imports =
        [ CodeGen.importStmt [ "Accessors" ]
            Nothing
            (CodeGen.exposeExplicit
                [ CodeGen.funExpose "makeOneToOne_"
                , CodeGen.typeOrAliasExpose "Lens"
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
                        [ ( fieldName, CodeGen.typeVar "attribute" ) ]
                    , CodeGen.typeVar "transformed"
                    , CodeGen.typeVar "attribute"
                    , CodeGen.typeVar "built"
                    ]
                    |> Just
            , implementation =
                let
                    { access, update } =
                        NoMissingRecordFieldLens.functionsForField fieldName
                in
                CodeGen.construct "makeOneToOne_"
                    [ CodeGen.string (String.cons '.' fieldName)
                    , access
                    , update
                    ]
            }
    }
