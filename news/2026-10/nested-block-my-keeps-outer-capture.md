# A nested block's `my` no longer hides the enclosing frame's lexical

A `my` in a nested block that shadows an enclosing frame's lexical gave the
routine's chunk a local of that name. The free-variable scan excludes every
local, so the routine never captured the outer variable, and every read of it
gave `(Any)`, including reads before the block:

```raku
my $b = 99;
sub w { say $b; { my $b = 3 }; say $b }
w();   # was (Any) (Any), now 99 99
```

A by-name read or write of the enclosing lexical in a named routine now forces
its capture. A routine hoisted to its scope's entry also counts the scope's
later declarations as enclosing lexicals (#13391).

Closure bodies are not covered yet. A closure's exit writes a captured name
back to its caller from the chunk's local slot of that name, which here is
the nested block's own `my`. A forced capture there would clobber the outer
variable, so the old `(Any)` remains until that writeback can tell the two
apart.
