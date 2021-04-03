let Github = { repo : Text, user : Text, token : Text }

in  { saunfDocPath = "./saunf/saunf.org"
    , readmePath = "./readme.md"
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
