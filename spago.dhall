{ name =
    "purescript-free-turtle"
, dependencies =
    [ "console"
    , "effect"
    , "free"
    , "math"
    , "maybe"
    , "canvas"
    , "prelude"
    , "psci-support"
    , "tuples"
    , "transformers"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
