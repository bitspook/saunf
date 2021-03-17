{ github = Some
  { user = "channikhabra", repo = "saunf", token = env:GITHUB_TOKEN as Text }
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
}
