# LTM: Unicode-property atoms and negated named classes end the declarative prefix

`<:L>`, `<-:L>` and `<-alpha>` have no NFA edge in Rakudo, so they are fates that end the
declarative prefix in alternation LTM. mutsu measured them as declarative, so
`"abx" ~~ /[ <:L>+ "x" | "ab" ]/` picked the first branch (`abx`) where Rakudo picks `ab`.
`ltm_atom_mode` now classifies both shapes as fates; `\D`, `<-[a]>` and positive named classes are
unchanged. Closes the first known gap in ADR-0111 §5.
