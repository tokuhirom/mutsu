# A `$*OUT` parameter redirects `say` / `print` / `note`

`sub f($*OUT) { say "x" }` wrote to the process's stdout, not to the handle
passed as the argument. `$*OUT.say` inside `f` already reached it (#11348).

mutsu binds a dynamic scalar under two env spellings, `*OUT` and `$*OUT`, and
the output builtins resolve their handle through `$*OUT` first. `my $*OUT`
writes both spellings, but the parameter binder wrote only `*OUT`. It now writes the
`$*OUT` twin too. `compute_declared_locals` counts each twin as being as
local as its name, so neither a parameter's nor a routine's `my $*OUT` twin is
merged back into the caller on return.
