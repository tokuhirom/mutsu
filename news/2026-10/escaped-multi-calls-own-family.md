# A lexical multi reached through `&name` can call its own family

A `multi sub` returned as `&name` from the scope that declared it used to die with
"Unknown function" when a candidate called its family by name. The dispatcher now binds
`&name` to itself in the candidate's frame for the call (#12321).
