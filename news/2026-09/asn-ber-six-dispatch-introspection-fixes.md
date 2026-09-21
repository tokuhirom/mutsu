# ASN::BER: six general dispatch/introspection fixes

Working `ASN::BER` 0.7.3 (`ecosystem-dist-fix` skill) took its ledger record from
`red` (0/16 baseline assertions) to `partial` (15/16): `t/00-sanity.t`,
`t/02-octet-string.t`, and `t/03-long-integers.t` now pass in full parity with
rakudo. None of the six root causes were about ASN.1 or BER encoding — every one
was a general interpreter bug that any distribution using the same Raku idiom
would hit.

1. **`trait_mod:<is>(Attribute, :$default!)` had no callable candidate.** mutsu
   only recognized `is default(...)` as parse-time sugar directly on a `has`
   line (`CompiledAttrDecl::is_default`). ASN::Types' own `DefaultValue` role
   composes a custom `is default-value(...)` trait and then re-dispatches to
   CORE's own default machinery at runtime:
   `trait_mod:<is>($attr, :default($default-value))`. That call found no
   candidate at all, and the "no candidate" verdict was misreported as the
   OUTER `is default-value` trait itself being unknown. Added the missing
   candidate as a new prelude (`TRAIT_MOD_IS_DEFAULT_PRELUDE`, following the
   existing `TRAIT_MOD_DOES_PRELUDE`/`TRAIT_MOD_IS_NATIVECALL_PRELUDE`
   pattern), backed by a native primitive (`__mutsu_attribute_set_default`)
   that relays the value through a new `trait_mod_default_writeback` field;
   `apply_class_body_attribute_traits` drains it right after dispatching an
   attribute's own custom traits and folds it into the attribute's compiled
   default.

2. **An uppercase-starting `is` trait on a `$`/`&`-sigil attribute was silently
   swallowed.** The parser's "uppercase trait name -> container type trait"
   branch (meant for `has @.a is Array[Int]` / `has %.h is BagHash`) fired for
   ANY sigil, so `has Str $.name is UTF8String;` never reached the custom
   `trait_mod:<is>` dispatch at all — no error, just a discarded trait. Gated
   the branch on `sigil == '@' || sigil == '%'`, matching its own doc comment;
   a `$`/`&`-sigil uppercase trait now falls through to the same
   `unknown_traits` path a lowercase one already uses (which itself resolves a
   real type name positionally when one exists).

3. **A sibling multi candidate's `where` clause could corrupt an unrelated
   `is rw` parameter bind.** `method_args_match_for_invocant`'s speculative
   candidate-matching window already saved/restored `env` and
   `current_package`, but not `pending_call_arg_sources`. Evaluating one
   candidate's `where` clause (`$enum-type.HOW ~~ Metamodel::EnumHOW`) runs a
   nested method dispatch that clears that field on its own exit — wiping out
   the arg-source names the candidate ACTUALLY selected needed moments later
   to bind its own `is rw` parameter, even though that parameter's argument
   was a perfectly ordinary lexical (`self.parse($in, $!type, ...)` in
   `ASN::Parser`). Added it to the same save/restore window.

4. **`.HOW` on an enum VALUE reported the wrong metaclass.** `Solid.HOW`
   (`Solid` an instance of `enum Fuel <Solid Liquid Gas>`) reported
   `Perl6::Metamodel::ClassHOW` instead of `EnumHOW`, even though `Fuel.HOW`
   (the type object) already resolved correctly. `dispatch_how`'s type-name
   resolution had a case for `Package`/`Instance`/`Mixin` but none for
   `ValueView::Enum` — an enum value's own dedicated representation — so it
   fell through to a catch-all reporting `Mu`. `ASN::Serializer`/`ASN::Parser`
   both dispatch on `$value.HOW ~~ Metamodel::EnumHOW` to tell an enum value
   apart from a plain `Int`, so the wrong metaclass silently picked the `Int`
   multi candidate instead (wrong output, not a crash).

5. **A typed flexible-arity candidate could lose to an untyped exact-arity
   catch-all.** Bare-name multi-sub resolution gathers the call's exact-arity
   candidates first and, as a performance fast path, returns their winner
   immediately whenever none of them has an optional positional parameter —
   reasoning that such a candidate is already narrower than any
   different-arity fallback. That reasoning silently breaks when the
   exact-arity winner is a fully untyped catch-all (`multi f($x) {...}`, the
   widest possible signature): `ASN::Serializer.serialize`/`ASN::Parser.parse`
   both declare a typed candidate with a trailing optional/default parameter
   (`Int $index = 2`) alongside such a catch-all, and the catch-all won every
   time. The fast path now also checks that the exact-arity winner is
   nominally typed before short-circuiting.

6. **Method dispatch's type-distance ranking misaligned positional parameters
   against named arguments.** `method_candidate_type_distance` indexed the
   call's raw argument list — positional and named values interleaved in
   call-site order — directly by a counter meant to walk only positional
   parameters. A candidate with an unfilled trailing optional positional
   parameter (`Int $index = 10`, no positional argument left for it) then read
   the NEXT array slot for that parameter's distance check, which — whenever
   the call also passed any named argument — was that named argument's `Pair`
   value rather than nothing. Scoring `Int` against a `Pair` came back
   "unrelated" and added a large penalty a same-arity untyped catch-all never
   paid, so `Serializer.serialize($value, ..., :$debug, :$mode)` picked its
   catch-all fallback every time a named argument was present, even after fix
   5 handled the no-named-argument case. Fixed by filtering the argument list
   to positional-only values before indexing.

Each fix carries its own focused regression test under `t/`. `t/01-async.t`
remains red: `ASN::Parser::Async.process` mutates its own `$!buffer` attribute
through a *different* object's (`$!parser`'s) `is rw` method parameter
(`$!parser.get-tag($!buffer)`), and that writeback is silently a no-op —
tracked as [#8904](https://github.com/tokuhirom/mutsu/issues/8904)
(`todo:deep`): the mutation writes back fine when the call target is `self`,
or when the argument is a plain lexical, but not for the combination of an
attribute argument and a non-`self` callee.
