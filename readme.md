# Saunf

Saunf is a plain text software project management tool.

Not everyone is a plain-text fan, so to enable collaboration, saunf can
integrate with different tools. Or it is supposed to.

I am building Saunf to simplify managing software projects for me, while
allowing people I work with to use the tools they like. At its infancy,
Saunf should enable my current workflow of managing a project from a
single [org](https://orgmode.org/) file:

-   I write a lot of introductory documentation. Saunf should allow me
    to pick parts from this document and build a readme file
-   I keep a list of todo items within the [saunf-doc](#saunf-doc).
    Saunf should allow me to sync these with Github issues.
-   I maintain a glossary for every project to establish a ubiquitous
    language. Saunf should enable me to utilize this glossary to more
    effectively achieve my goal of establishing a ubiquitous language.

I have a lot of (rough) fantasies of expanding the scope to replace
Jira, and a lot more than that. I imagine a tool which can allow me to
see a project from bird's eye view as architecture diagrams, to which I
can then zoom into get more detail, zooming in further and further to
end up reading source code. A tool which would allow me to trace every
single line of code, every domain object to the requirements that
demanded its existence. Projects where things are automatically
connected.

For starters, let's try to bring together the information that usually
spreads over a gazillion tools with no convergence point.

## Features

### Documentation Management

-   Reduce busy work of keeping multiple tools up to date e.g a person
    shouldn't need to put time in updating a wiki (e.g confluence), a
    public readme (e.g on github) etc with same information
-   Help establish a ubiquitous language to discuss the project
    -   Easily maintain a glossary of what certain terms mean in the
        project
    -   Easily/automatically link the glossary terms whenever they are
        used elsewhere in documentation, or issues, commit messages,
        branch names etc
-   Create a documentation website?

### Issue Management

Help manage the user-stories, bugs, technical debts and such.

-   Creating a new issue should be as easy as dropping in a line in the
    saunf-doc
-   Support syncing the issues with an external tool
    -   Issues added/modified in saunf-doc should get pushed to the
        remote tool
    -   Issues added/modified in remote tool should be brought back to
        the saunf doc

## MVP

At its bare minimum, Saunf should

-   sync readme file
    -   \[X\] Push changes from saunf-doc to readme
    -   \[ \] Pull changes from readme to saunf-doc
-   sync github issues
    -   \[ \] Push new issues to github
    -   \[ \] Push changes to github
    -   \[ \] Pull new issues from github
    -   \[ \] Pull changes from github
