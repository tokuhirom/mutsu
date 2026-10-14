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

Declaring such a block-local `my` now forces the capture of the enclosing
lexical. A loop or pointy parameter binding is excluded, because it binds by
name and would write through the captured cell. A routine hoisted to its
scope's entry also counts the scope's later declarations as enclosing
lexicals (#13391).
