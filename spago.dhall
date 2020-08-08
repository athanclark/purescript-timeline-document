{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "numbers"
  , "psci-support"
  , "quickcheck-utf8"
  , "spec"
  , "timeline-identifiers"
  , "timeline-time"
  , "unique-array"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
