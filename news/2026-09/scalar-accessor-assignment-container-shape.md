# .= through a method-call accessor no longer preserves the prior container shape

`self.x = [a => 1, b => 2]; self.x .= Hash` left `self.x` an Array of the
Hash's pairs instead of the Hash itself.

`normalize_rw_accessor_assignment` (in `src/runtime/methods_mut.rs`) decided
whether an assignment through a method-call lvalue target should flow INTO
the existing container (preserving its shape and `is default(...)` --
correct for `@`/`%`-sigil accessors) purely from the current value's runtime
shape, never from the attribute's actual declared sigil. A `$`-sigil
(scalar) attribute whose stored value happened to be an Array (from an
earlier `self.x = [...]`) was therefore treated exactly like an `@`-sigil
one: every later assignment through the accessor -- including a `.=`
coercion's own already-correct result -- was silently coerced back into an
Array.

A `$`-sigil accessor REBINDS on assignment in Raku; it never flows into
whatever the attribute currently holds (verified against `raku`:
`has $.a is rw is default(99) = [1,2,3]; $obj.a = <x y>` gives a plain
`List`, no default carried, no Array coercion).

Fixed by replacing `normalize_rw_accessor_assignment`'s
`force_hash_context: bool` parameter with the attribute's actual
`attr_sigil: char`, threaded through from all four call sites in
`src/runtime/methods_mut_method_lvalue.rs`. The container-shape-preserving
branches now only run for `@`/`%`; a `$` sigil returns the incoming value
unchanged. Pinned by
`t/oo/attribute/scalar-accessor-assignment-does-not-preserve-container-shape.t`.

Two related but distinct gaps were found while investigating this and filed
separately, since their root causes differ from this coercion bug: #9022 (a
native-int scalar accessor assigned through this same method-call-lvalue
path never wraps on overflow) and #9023 (a Hash/Array literal assigned
through a `$`-sigil accessor is not itemized).

Closes #9005.
