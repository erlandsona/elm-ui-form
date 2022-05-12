module NoMissingConstructorPrism exposing (rule)

{-|

@docs rule

-}

import Dict
import Elm.CodeGen as CodeGen
import Elm.Pretty exposing (prettyDeclaration)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Pretty exposing (pretty)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports... REPLACEME

    config =
        [ NoMissingConstructorPrism.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template undefined/example --rules NoMissingConstructorPrism
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoMissingConstructorPrism" ()
        -- Add your visitors
        -- |> Rule.fromModuleRuleSchema
        |> Rule.withDeclarationListVisitor generatePrismFromTypeDecl
        |> Rule.fromModuleRuleSchema


generatePrismFromTypeDecl : List (Node Declaration) -> moduleContext -> ( List (Error {}), moduleContext )
generatePrismFromTypeDecl nodes ctx =
    ( List.foldr
        (\(Node injectionRange node) acc ->
            case node of
                FunctionDeclaration fn ->
                    let
                        fnName : String
                        fnName =
                            fn.declaration
                                |> Node.value
                                |> .name
                                |> Node.value

                        ctorName : String
                        ctorName =
                            fnName |> String.dropLeft 2
                    in
                    if fnName |> String.startsWith "c_" then
                        Dict.insert ctorName Nothing acc

                    else
                        acc

                CustomTypeDeclaration type_ ->
                    let
                        isAdt : Bool
                        isAdt =
                            -- If custom type only has one constructor then
                            -- we can generate a Lens.
                            List.length type_.constructors > 1
                    in
                    if isAdt then
                        List.foldr
                            (\(Node errorRange ctor) ->
                                let
                                    ctorName : String
                                    ctorName =
                                        Node.value ctor.name
                                in
                                if List.length ctor.arguments > 0 then
                                    Dict.update ctorName
                                        (\m ->
                                            case m of
                                                Nothing ->
                                                    Just
                                                        (Just
                                                            (Rule.errorWithFix
                                                                { message = "Generating a `c_" ++ ctorName ++ "` Prism for the type constructor: `" ++ ctorName ++ "`."
                                                                , details = [ "missing prism for constructor `" ++ ctorName ++ "`" ]
                                                                }
                                                                errorRange
                                                                [ Fix.insertAt
                                                                    { row = injectionRange.end.row + 1
                                                                    , column = injectionRange.end.column
                                                                    }
                                                                    ("\n\n\n"
                                                                        ++ printPrism ctor
                                                                    )
                                                                ]
                                                            )
                                                        )

                                                otherwise ->
                                                    otherwise
                                        )

                                else
                                    identity
                            )
                            acc
                            type_.constructors

                    else
                        acc

                _ ->
                    acc
        )
        Dict.empty
        nodes
        |> Dict.values
        |> List.filterMap identity
    , ctx
    )


printPrism : ValueConstructor -> String
printPrism ctor =
    let
        ctorName : String
        ctorName =
            Node.value ctor.name

        prism super =
            CodeGen.typed "Relation"
                [ super
                , CodeGen.typeVar "sub"
                , CodeGen.typeVar "wrap"
                ]

        { access, update } =
            implementation ctor

        fnName : String
        fnName =
            "c_" ++ ctorName
    in
    CodeGen.funDecl
        Nothing
        (CodeGen.funAnn
            (prism (CodeGen.typeVar "sub"))
            (prism (CodeGen.typeVar "sub"))
            |> Just
         -- c_Data : Relation b reachable a -> Relation (Async b) reachable (Async a)
         -- TODO: Upgrade once I figure out a `type alias Optional`
         -- CodeGen.typed "Lens"
         --     [ CodeGen.extRecordAnn "record"
         --         [ ( fieldName, CodeGen.typeVar fieldName ) ]
         --     , CodeGen.typeVar fieldName
         --     , CodeGen.typeVar "sub"
         --     , CodeGen.typeVar "wrap"
         --     ]
         --     |> Just
        )
        fnName
        []
        -- TODO: Add import Accessors exposing (makeOneToOne) or whatever for any
        -- modules we're generating code for.
        (CodeGen.construct "makeOneToOne"
            [ CodeGen.string fnName
            , access
            , update
            ]
        )
        |> prettyDeclaration 100
        |> pretty 100


implementation :
    ValueConstructor
    ->
        { access : CodeGen.Expression

        -- , set : CodeGen.Expression
        , update : CodeGen.Expression
        }
implementation ctor =
    let
        ctorName : String
        ctorName =
            Node.value ctor.name
    in
    { access =
        CodeGen.lambda
            [ CodeGen.varPattern "t" ]
            (CodeGen.caseExpr (CodeGen.val "t")
                [ ( CodeGen.namedPattern ctorName
                        (List.indexedMap
                            (\ix _ ->
                                CodeGen.varPattern ("a" ++ String.fromInt ix)
                            )
                            ctor.arguments
                        )
                  , CodeGen.construct "Just" [ CodeGen.val "a0" ]
                  )
                , ( CodeGen.allPattern, CodeGen.val "Nothing" )
                ]
            )
    , update =
        -- TODO: Maybe generate Settable version instead of just the mapping version?
        CodeGen.lambda
            [ CodeGen.varPattern "fn"
            , CodeGen.varPattern "t"
            ]
            (CodeGen.caseExpr (CodeGen.val "t")
                [ ( CodeGen.namedPattern ctorName
                        (List.indexedMap (\ix _ -> CodeGen.varPattern ("a" ++ String.fromInt ix)) ctor.arguments)
                  , CodeGen.construct ctorName [ CodeGen.parens (CodeGen.construct "f" [ CodeGen.val "a0" ]) ]
                  )
                , ( CodeGen.namedPattern "otherwise" [], CodeGen.val "otherwise" )
                ]
            )
    }
