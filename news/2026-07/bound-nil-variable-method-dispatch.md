# Bound-Nil variables reach the same method verdicts as literal Nil

A variable *bound* (not assigned) to `Nil` — `my $v := Nil` — dispatched
methods through the named-receiver opcode (`vm/vm_call_method_mut_ops.rs`),
which skipped the Nil special-casing that the scalar `MethodCall` opcode
carries: `$v.Numeric` / `$v.Int` silently absorbed to `Nil` and `$v.Str`
returned `""` without a warning, where raku warns "Use of Nil in
numeric/string context" and resumes with the zero / empty string
(`Nil.Int` in the literal form already behaved correctly).

The warn-and-resume coercion arms (numeric coercions to their typed zero,
`abs`/`floor`/`ceiling`/`round`/`truncate`/`sign` to 0, `Str`/`Stringy` to
`""`, `ords`/`chrs`) and the element-mutator errors (`BIND-POS`/`STORE`/...)
were extracted out of the scalar opcode's inline `is_nil` block into a shared
`nil_predispatch_error()` (`vm/vm_call_method_ops.rs`), and the named-receiver
opcode now consults it before normal dispatch. Everything else keeps its
existing route: autovivification (`push`/`append`/...), the methods Nil
genuinely defines (`.WHAT`, `.defined`, `.gist`, `&?BLOCK.leave`, exception
accessors), and the post-dispatch `Nil.FALLBACK` absorb for unknown methods.

Found while fixing `.Int` on Any/Mu type objects
(news/2026-07/any-type-object-int-coercion.md). Pinned by
`t/bound-nil-method-warn.t`.
