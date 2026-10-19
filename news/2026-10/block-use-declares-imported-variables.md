# A block's `use` declares the variables it imports in the block

A `use` inside a block, loop body or routine now declares each `our`
variable it imports as the block's own binding of the module's variable,
as rakudo installs it in the block's lexical pad (ADR-8751 stage 1,
slice 3). Inside the block the import hides an outer variable of the same
name, a closure taken there keeps it after the block is gone, and a routine
written outside the block reads its own variable, not the import:

```raku
our $level = 'outer';
sub report { $level }
{
    use Logger;           # exports `our $level is export`
    say $level;           # the module's, not 'outer'
    say report();         # 'outer'
}
```

Before, the import wrote the variable into the block's env by name. An
outer `$level` declared in a slot won inside the block, a routine called
from the block found the import through the caller chain, and after a loop
body's `use` the outer variable kept the module's value for good.

The compiler learns which variables a `use` imports from the parse-time
module scan, so a module that computes its exports in `sub EXPORT` keeps the
old run-time import.

Two bugs the change exposed are fixed with it: a mainline block's own
`my %h` no longer stores elements into a file-scope `%h` that a named sub
captured, and a `use` in a routine body called repeatedly no longer leaves
the module's package visible after the routine returns.
