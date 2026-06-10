use Test;

plan 4;

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
