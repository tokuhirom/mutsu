# A constructor-supplied `Nil` now resets an attribute to its type default

`Class.new(:attr(Nil))` stored the raw `Nil` into the attribute instead of
resetting it to the container's declared type default, the way a plain
`$x = Nil` assignment already did correctly. `C.new(:m(Nil)).m.raku` read
`Nil` where Rakudo reads `Any`, and a typed attribute (`has Int $.t`) got
`Nil` instead of the `Int` type object.

The two named-argument-to-attribute paths in `dispatch_bless`
(`src/runtime/methods_dispatch_new.rs`) — the pre-seed pass that lets a later
default read an earlier-supplied attribute, and the override pass that
applies named args after defaults — now reuse the same `seed_attr_value`
helper the no-initializer and `= Nil`-literal-default cases already used, so
a caller-supplied `Nil` is seeded exactly like those. The native default-ctor
fast path (`build_native_default_instance` in
`src/runtime/methods_object_default_ctor.rs`) got the matching fix for its
untyped-attribute arm; the typed-attribute arm already fell through to the
interpreter on a `Nil` (which failed its type check), so it now lands on the
newly-fixed `dispatch_bless` path instead of storing `Nil` verbatim.

Found while working #9491 (ANTLR4::Grammar), whose generated code passes
`Nil` through positional constructor arguments routinely. (#9676)

## Unmasked: the coercion-fallback `new` never got its context

Fixing the Nil-storage bug turned up a second, unrelated gap: whitelisted
`roast/S12-coercion/coercion-methods.t`'s "method new has its context set"
subtest started failing. `class C1 { has Mu $.coercion-type; multi method
new(::?CLASS:U: Bar:D $bar) { self.new: :coercion-type($*COERCION-TYPE) } }`
reads the dynamic variable `$*COERCION-TYPE`, which mutsu never declared or
bound anywhere — it silently resolved to `Nil` (mutsu treats a wholly
undeclared `$*name` as `Nil` rather than raising, a separate pre-existing
looseness). Storing that raw `Nil` happened to satisfy `isa-ok`'s coercion-type
check (`Nil.isa(Any)` is true, and an empty-source coercion type like
`C1(Any)` matches any defined value for that check) — the test was passing
for an accidental reason. Once the Nil-storage fix seeds the attribute's
declared type object (`Mu`, which does not `isa(Any)`) instead, that accident
stopped covering for the missing feature.

`try_coerce_value_with_method`'s fallback-to-`new` call (used when no
`COERCE` candidate matches, `src/runtime/types/coercion.rs`) now binds
`$*COERCION-TYPE` to the coercion's target type for the duration of that one
call, the same save/insert/restore shape `indir`'s `$*CWD` binding already
uses for a builtin dynamic variable no user code lexically declares. Pinned
in `t/types/coercion/coercion-fallback-new-context-type.t`.
