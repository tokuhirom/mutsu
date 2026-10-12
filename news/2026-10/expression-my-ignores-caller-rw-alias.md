# An expression-position `my` no longer writes through a caller's `is rw` parameter

`sub p($w) { (my Str $str = $w) }` called from `sub go(Str $str is rw) { p("y") }`
wrote `"y"` into `go`'s argument on the second call and read its own `$str` back as
`(Any)`. The caller's typed `is rw` parameter leaves a sigilless-alias entry
(`$str` → the argument's variable) in its frame, and the callee's slot-less
declaration store reached it through the dynamic env chain: both the sigilless-alias
write-through and the alias propagation at the end of `SetGlobal` followed it.

A store that is a fresh declaration (`fresh_binding_decl`) now skips every by-name
write-through (the `ContainerRef` cell, the escaped-`our` cell, the sigilless alias)
and the alias propagation. It also hides the inherited alias key in its own tier, so a
later plain `$str = ...` in the callee stays in the callee's binding. This unblocks
#13001, whose frame roots for closure-creating methods exposed the same shape in
Text::CSV's `Text::IO::String.print` (#13007).
