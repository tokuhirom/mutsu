---
title: Preserve argumented regex subrules in RakuAST
---

Argumented regex subrules now retain Rakudo's `Named::Args` assertion and
`ArgList` tree in `.AST`, including colon syntax, dot suppression, and
qualified names. Constructed trees lower through the existing package-aware
regex matcher and preserve match-time argument behavior.
