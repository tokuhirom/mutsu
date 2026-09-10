use Test;

plan 6;

# `use MONKEY-TYPING` is lexical: a `use` inside a block does not enable
# augmentation outside that block.
throws-like '{ use MONKEY-TYPING; }; augment class Any { }',
    X::Syntax::Augment::WithoutMonkeyTyping,
    'MONKEY-TYPING applies lexically';

# Without any MONKEY-TYPING, augment is rejected.
throws-like 'augment class Any { }',
    X::Syntax::Augment::WithoutMonkeyTyping,
    'augment without MONKEY-TYPING is rejected';

# MONKEY-TYPING in scope (same block) allows augmentation.
is EVAL('{ use MONKEY-TYPING; augment class Int { method ttt { 7 } }; 3.ttt }'),
    7, 'augment works inside the MONKEY-TYPING block';

# Top-level MONKEY-TYPING enables augmentation for the rest of the unit.
is EVAL('use MONKEY-TYPING; augment class Int { method uuu { 9 } }; 5.uuu'),
    9, 'top-level MONKEY-TYPING enables augmentation';

# EVAL is a fresh compilation unit for this (compile-time-checked) pragma:
# a `use MONKEY-TYPING` active in the CALLER's lexical scope does not extend
# into a separately EVAL'd string, unlike a runtime dynamic-scope pragma such
# as `fatal` (verified against `raku -e 'use MONKEY-TYPING; try { EVAL q[class
# C { method f {} }; augment class C { method f {} }] }; say $!.^name'` ->
# X::Syntax::Augment::WithoutMonkeyTyping, not the method-clash error an
# inherited pragma would reach).
use MONKEY-TYPING;
try {
    EVAL 'class MonkeyC { method f { 1 } }; augment class MonkeyC { method f { 2 } }';
}
is $!.^name, 'X::Syntax::Augment::WithoutMonkeyTyping',
    "an outer use MONKEY-TYPING doesn't leak into a separately EVAL'd string";

# ...and an EVAL'd string that turns MONKEY-TYPING on itself still works, even
# after the caller has already `use`d it (the `loaded_modules` fast path that
# re-arms `strict`/`fatal` on a repeat `use` of an already-loaded module had
# no matching arm for MONKEY-TYPING, so this regressed while fixing the case
# above -- see the `loaded_modules.contains(module)` branch in
# `runtime_module.rs`).
is EVAL('use MONKEY-TYPING; augment class Int { method vvv { 11 } }; 5.vvv'),
    11, "an EVAL'd string's own use MONKEY-TYPING still works after the caller already used it";
