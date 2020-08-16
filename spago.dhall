{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "comonad-ui"
, dependencies =
  [ "console", "effect", "prelude", "psci-support", "transformers", "day", "smash" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
