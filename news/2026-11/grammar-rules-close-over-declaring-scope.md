# Grammar rules close over the scope they are declared in

A token, rule or named regex now reads the lexicals of the routine or block
its grammar (or the `my regex` itself) is declared in, as a method already
did (#17872):

```raku
sub mk() { my $m = "q"; my grammar K { token TOP { x <?{ $m eq "q" }> } }; K }
say so mk().parse("x");   # True (was False, with "uninitialized value")
```

A grammar resolves a rule by name and matches its re-parsed pattern in the
env of whoever runs the match, so once the declaring scope had returned, its
lexicals were unbound there. Now, when the grammar (or the `my regex`) is
registered, the declaring frame's variables each rule's pattern names are
boxed into shared cells and kept per rule (`Registry::token_captures`,
`capture_declared_token_envs`), the rule analogue of
`capture_declared_method_envs`. They are bound for the rule's
resolve-and-match window and for its body render
(`regex_token_closure`). Code blocks, assertions and interpolated variables
see them, and a write reaches the declaring scope.
