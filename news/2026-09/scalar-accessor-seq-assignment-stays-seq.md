# A Seq assigned to a scalar attribute through its accessor stays a Seq

Assigning a `Seq` to a `$`-sigil attribute through its accessor used to turn
it into an Array. Raku keeps it a `Seq` -- a `$` container records itself on
the Seq *handle* rather than replacing the value, so the thing stays a `Seq`
to every consumer while `.raku` renders the container:
`class E { has $.w is rw }; E.new.w = (1,2,3).Seq; ....w.raku` now gives
`$((1, 2, 3).Seq)`, matching rakudo, instead of `$[1, 2, 3]`.

Two separate bugs combined to produce that behavior:

1. `builtin_assign_method_lvalue` (`src/runtime/builtins_multidim_assign.rs`)
   unconditionally ran every Seq rvalue through `coerce_to_array` before the
   store, for every attribute sigil -- not just `@`/`%`, whose in-place
   container refill (`replace_container_contents`) genuinely needs a concrete
   Array/Hash. A new `Interpreter::method_lvalue_is_scalar_attr` helper
   (`src/runtime/methods_mut_rw_attr.rs`) now identifies a `$`-sigil target
   (the generated accessor or a bare-`$!attr` `is rw` method) so the eager
   coercion is skipped for exactly that case; the lazy Seq is still forced
   (`reify_map_grep_seq`) so a `.grep`/`.map` callback cannot leak past the
   store, it just isn't flattened into an Array afterward.

2. Once that blanket coercion was narrowed, a `$`-held Seq turned out to be
   silently *consumed* by the assignment statement's own implicit sink
   (`$obj.w = SEQ;` discards its result at the statement level): a real
   Raku `Seq` survives that sink only when it is itemized, but
   `itemize_scalar_store_value` (`src/vm/vm_run_loop.rs`) only retagged the
   Seq's view -- it never called `SeqBody::mark_itemized()`, the flag that
   actually grants the exemption. The local-variable `SetLocal` store path
   made that call itself, beside its own use of the same itemize function;
   the attribute-accessor store had no such call of its own, so the Seq
   vanished (rendering `Seq.new()`) on its very first read. `itemize_scalar_store_value`
   now marks the body itemized itself, fixing every caller at once.

Pinned by `t/oo/attribute/scalar-accessor-seq-assignment.t`, covering the
generated accessor, a bare-`$!attr` `is rw` method, a grep-derived Seq, the
already-correct Slip/Range/List cases, and the `@`/`%` in-place-coercion
cases that must NOT change.

Closes #9042.
