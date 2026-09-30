# A grammar's own `alpha`/`digit` now replaces the built-in in `<+alpha ...>` classes

A named item in a composite character class is a call to the rule of that name, so a token a
grammar defines under a built-in class's name (`token digit { 'x' }`) replaces the built-in in
`<+digit>`, `<+alpha -[q]>` and `<-digit>`. Before, the built-in was tested first and the token only
consulted when it failed, so `G.parse("1")` still matched. The built-in applies only when the
grammar does not override it. The ADR-0099 Stage 1 prefilter no longer narrows a class by a negated
item that the grammar overrides. Closes #10305.
