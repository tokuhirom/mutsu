# `.^can` lists one candidate per built-in class that declares the method

`.^can` answers one candidate for every class in the MRO that declares the method,
most-derived first. User classes already worked (`B.^can("m")` for `class B is A` with
both declaring `m` is 2), but a built-in level only ever contributed a single fallback
entry, so `Str.^can("uc").elems` was 1 where Rakudo says 2 (Str's own `uc` and Cool's),
and `Int.^can("Str")` was 1 where Rakudo says 2 (Int and Mu)
([#9869](https://github.com/tokuhirom/mutsu/issues/9869)).

The native method catalog (`src/builtins/native_method_row_table.rs`) could not answer
the question: its `INTROSPECTABLE` bit records which names an owner *responds to*, so it
is set on Cool, Any and Mu for `Str` even though only Int and Mu declare it. Treating it
as a declaration made `Int.^can("Str")` 4.

The catalog now carries a separate `DECLARED` flag, baked once from Rakudo: set on a
`(owner, name)` row exactly when `::(owner).^method_table` has the name — the per-class
table Rakudo's `.^can` walks. `collect_can_methods` asks `native_method_declared` at each
MRO level (matching the owner exactly, never folding `Sub`/`Routine`/`Block` onto `Code`)
and appends one candidate per declaring built-in class after the user-class candidates
and attribute accessors, so `class C { has $.Str }; C.^can("Str")` is C's accessor then
Mu's `Str`. Pinned by `t/oo/mop/can-built-in-mro-candidates.t`.
