let Github = { repo : Text, user : Text, token : Text }

let readmePath = "./readme.md"

let saunfDocPath = "./saunf/saunf.org"

in  { saunfDocPath
    , readmePath
    , readmeTemplate = Some
        ''
        # $title$

        $description$

        ## Features

        ### $#doc-management-module$
        ### $#issue-management-module$

        ## $#mvp$

        ## $#usage$
        ''
    , github = Some
      { user = "channikhabra"
      , repo = "saunf-test"
      , token = env:GITHUB_TOKEN as Text
      }
    }
