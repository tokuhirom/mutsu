# `use worries` is a recognized pragma

`use worries` used to die at run time with "Could not find worries". It is a
core lexical pragma: it re-enables the compiler's "Potential difficulties"
warnings that an enclosing `no worries` turned off, for the rest of its own
scope. The parser now clears the scope's `no worries` flag on `use worries`
(the counterpart of the existing `suppress_worries`), and the compiler and the
runtime's module loader treat it as a pragma rather than a module to load.

`use trace`, reported alongside it in #10480, needs a per-statement tracing
hook numbered the way Rakudo numbers statements; it is tracked separately in
#10613.
