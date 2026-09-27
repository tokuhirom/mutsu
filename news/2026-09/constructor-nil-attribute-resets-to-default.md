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
