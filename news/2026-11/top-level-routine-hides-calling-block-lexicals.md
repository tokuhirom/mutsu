# A top-level routine no longer reads a calling block's lexicals

A routine declared at the top of the program resolved some names through whatever mainline block
called it. With `sub f(:&cmp = &[cmp]) { cmp(2, 1) }` called from a block that declares
`my &infix:<cmp>`, the default picked up the block's operator (`inner`; raku: `More`), and so did a
body's `&infix:<cmp>(2, 1)` or `my &c = &[cmp]` (#17630).

Such a routine's frame already links past its callers to the program scope (ADR-12529 phase 3),
but a mainline block is part of that scope's chain while it runs, so the link landed on the block.
Routines that look names up by string already hid the unit's block-only lexicals; now every
top-level routine hides the ones its code can name -- a free variable, a routine or operator name
an op carries, a name in a parameter's default or `where` -- and a routine that names none of them
carries no set and pays nothing per call. A parameter default is also evaluated with the static
link honoured: the binder around it reads the caller's argument sources through the whole chain,
but the default is code of the routine's own scope.
