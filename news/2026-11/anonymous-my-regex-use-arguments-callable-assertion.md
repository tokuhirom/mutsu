# Anonymous `my regex`; origins of use arguments and callable assertions

An anonymous regex declarator with a scope (`my regex ($x) { $x }`,
`my token { ... }`) was a parse error ("Two terms in a row"): the `my`
dispatch only knew the named form. It is now the same term as the unscoped
declarator makes, so `<&$p("b")>` can call it.

`.origin` (#15780):

- the argument list of a `use` statement (`use Test :DEFAULT, :ALL`) is
  placed, parsed again from the text after the module name
  (`use_origins::arguments`);
- an anonymous `my regex` runs from its `my`, and the initializer holding it
  is placed;
- a callable regex assertion with arguments (`<&$p("b")>`, `<&q("b")>`)
  places its callee (an item contextualizer over `$p`, or `&q`) and its
  `ArgList` (`regex_alias_origins::callee`).
