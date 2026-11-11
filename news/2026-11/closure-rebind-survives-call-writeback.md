# A closure's `:=` of a captured hash survives an earlier call

A variable that a closure rebinds with `:=` gets a *binding cell* when it is
declared: a cell whose content is the variable's container, shared by the
declaring frame's slot and every closure that captures it, so the rebind
reseats the binding everyone sees (ADR-12529 §3).

Passing such an `@`/`%` variable to a routine's `$` parameter broke that. On
return, the call's argument writeback stored the parameter's final value -- the
bare Hash -- *into the binding cell itself*, so the cell became an ordinary
container. A later `%state := $_` inside the closure no longer found a binding
cell, minted a fresh one in its own env and the declaring frame kept the old
hash:

```raku
my %state = count => 0;
my &ws = -> $_ { %state := $_ };
sub copy($value) { 1 }
copy(%state);
ws({count => 5});
say %state;    # {count => 5}; mutsu printed {count => 0}
```

The writeback now stores through the binding cell's *value* cell, the same rule
read-modify-write ops already follow (#10826), and leaves the binding intact.
EEL 0.3's fork machinery restores `:my %state` through exactly such a closure
(#17454).
