A `my class` declared by an imported module no longer remains in the importing
compilation unit's namespace or shadows a same-named local declaration. The
declaring module can still use the private type, including after its load has
finished. Exported lexical classes remain available to importing code as well.
(#8120)
