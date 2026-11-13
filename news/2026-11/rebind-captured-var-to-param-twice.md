# A sub that binds a captured variable to its parameter can run twice

`my $x; sub f($l) { $x := $l }; f(1); f(2)` used to die on the second call with "Cannot assign to a readonly variable or a value" (#17641). After the first bind, the binding cell of `$x` was the readonly parameter, and the store path refused any rebind of a readonly free-variable binding unless it was bound to a value or a type object. A `:=` rebind replaces the earlier binding wholesale, so it is now allowed whatever that binding was. Assigning to `$x` while it is bound to the readonly parameter still fails, as in rakudo.

Found through a REPL session: Jupyter::Kernel-style sandboxes call a host routine that binds a host lexical from every evaluated line.
