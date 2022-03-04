let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20211111/packages.dhall
        sha256:7ed6350fe897a93926d16298e37d2324aabbe5eca99810204719dc3632fb555f

in  upstream
  with grid-reactors =
    { dependencies =
      [ "aff"
      , "arrays"
      , "canvas-action"
      , "colors"
      , "console"
      , "effect"
      , "exceptions"
      , "foldable-traversable"
      , "free"
      , "halogen"
      , "halogen-hooks"
      , "halogen-subscriptions"
      , "heterogeneous"
      , "integers"
      , "maybe"
      , "partial"
      , "prelude"
      , "psci-support"
      , "random"
      , "st"
      , "tailrec"
      , "transformers"
      , "tuples"
      , "web-events"
      , "web-html"
      , "web-uievents"
      ]
    , repo = "https://github.com/Eugleo/purescript-grid-reactors.git"
    , version = "14152b2c94fe0df8a1030864c07c5a6ccdd05f34"
    }
