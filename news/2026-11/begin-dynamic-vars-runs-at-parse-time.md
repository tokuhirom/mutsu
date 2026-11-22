# A `BEGIN` that sets `%*ENV` runs at parse time, ahead of the next `use`

A top-level `BEGIN` that reads or writes a dynamic variable now runs while the
program is parsed, like a `BEGIN` that touches nothing of the program already
did:

```raku
BEGIN %*ENV<MY_MODULE_DEBUG> = 1;   # runs here, at parse time
use MyModule;                        # loads after it, still at parse time
```

Before, a dynamic variable counted as something the `BEGIN` needed the
program's frame for, so it waited for the BEGIN-time prologue after the parse,
and so did every `use` after it. The module then loaded only after the whole
program parsed, which lost its load-time output to any later syntax error and
left the parse with a static scan of the module.

A `BEGIN` still waits for the prologue when the program declares a dynamic
variable of its own ahead of it (`my $*X` is in the program's frame, which the
`BEGIN` sees in its static state, as on rakudo), and when it reads the
compiler's own dynamics (`$*R`, `$*CU`).
