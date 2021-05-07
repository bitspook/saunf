let VerbosityLevel = < Error | Warning | Info | Debug >

let Github = { repo : Text, user : Text, token : Text }

let readmePath = "./readme.md"

let saunfDocPath = "./saunf.org"

let verbosity = VerbosityLevel.Info

in  { saunfDocPath
    , readmePath
    , readmeTemplate = Some
        ''
        * $title$

        $description$

        ** Features

        *** $#doc-management-module$
        *** $#issue-management-module$

        ** $#mvp$

        ** $#usage$
        ''
    , github = Some
      { user = "channikhabra"
      , repo = "saunf-test"
      , token = env:GITHUB_TOKEN as Text
      }
    , verbosity
    }
