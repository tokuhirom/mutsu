# A nested `my sub` may share its enclosing sub's name inside EVAL

`EVAL 'sub f { my sub f { 1 }; f() }'` raised a false "Redeclaration of routine"
because the lexical-shadow exemption was switched off for everything inside an
EVAL. The exemption now also applies to a declaration executing in a routine
that the EVAL'd code called (deeper than the routine stack was when the EVAL
began), while an EVAL's own top-level redeclarations are still rejected.

This unblocks `Services::PortMapping`'s `t/00-load.t`: `use-ok` EVALs a `use`
whose module chain reaches Slangify's `sub EXPORT { my sub EXPORT { ... } }`.
