# A block `use` declares a package-less module's exported `our` variables

A module with no `unit` or package declaration exports its `our` variables and
constants as `GLOBAL` ones, and the parse-time scan skipped them. A block-scoped
`use` of such a module therefore left `$x` resolving to a caller's same-named
`my $x` slot, which tripped the env/slot invariant (a panic on a debug build).
The scan now lists them like a packaged module's, so the block declares the
import and it hides the caller's lexical (#14436).
