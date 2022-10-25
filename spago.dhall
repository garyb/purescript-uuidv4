{ name = "uuidv4"
, dependencies =
  [ "arrays"
  , "assert"
  , "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "gen"
  , "integers"
  , "maybe"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "random"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
