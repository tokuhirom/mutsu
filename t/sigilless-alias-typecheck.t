use Test;

# Pin for todo/deep/sigilless-alias-assignment-skips-type-constraint.md:
# writing to a typed scalar THROUGH a sigilless `\x := $a` bind alias (or a
# sigilless routine parameter that aliases a typed caller variable) used to
# skip the type check entirely, because `OpCode::TypeCheck` emission was
# driven by a compile-time name-keyed map populated only for the DECLARED
# variable's own name -- `x` never got an entry, so no check ever ran for a
# store reaching `$a` through the alias.
#
# The fix checks the runtime, name-keyed `var_type_constraint` registry at
# the point a value is mirrored into an alias TARGET's storage (the
# `__mutsu_sigilless_alias::` forward-chain write, used both by same-scope
# `:=` binds and by sigilless routine-parameter aliasing), not just at a
# direct same-name store.
#
# NOTE: the assignment under test is wrapped in `try { }`, not `throws-like
# { }` -- `throws-like`'s block is a genuine closure, and writing through a
# sigilless alias CAPTURED into a closure goes through a different (cell-
# based) write-through mechanism this fix does not reach; see the ticket's
# residual scope. A `try { }` block is not a closure boundary, so it exercises
# the same direct-store code path as an ordinary statement.

plan 6;

subtest 'direct := bind alias to a subset-typed scalar' => {
    plan 2;
    subset SmallInt of Int where -128 <= $_ <= 127;
    my SmallInt $a = 5;
    my \x := $a;
    try { x = 1000 };
    isa-ok $!, X::TypeCheck::Assignment,
        'assigning through the sigilless alias raises the subset type check';
    is $a, 5, 'the source variable is unchanged after the rejected write';
}

subtest 'direct := bind alias to a plain Int-typed scalar' => {
    plan 2;
    my Int $a = 5;
    my \x := $a;
    try { x = "not an int" };
    isa-ok $!, X::TypeCheck::Assignment,
        'assigning a Str through the alias raises a type check';
    is $a, 5, 'the source variable is unchanged after the rejected write';
}

subtest 'a valid write through the alias still succeeds' => {
    plan 2;
    my Int $a = 5;
    my \x := $a;
    x = 42;
    is $a, 42, 'the source variable observes a type-matching write';
    is x, 42, 'the alias itself reads the new value';
}

subtest 'sigilless routine parameter aliasing a typed caller variable' => {
    plan 2;
    subset TinyInt of Int where -128 <= $_ <= 127;
    sub f(\x) { x = 1000 }
    my TinyInt $a = 5;
    throws-like { f($a) }, X::TypeCheck::Assignment,
        'writing through a sigilless param alias raises the subset type check';
    is $a, 5, "the caller's variable is unchanged after the rejected write";
}

subtest 'a valid write through a sigilless routine parameter still succeeds' => {
    plan 1;
    sub f(\x) { x = 42 }
    my Int $a = 5;
    f($a);
    is $a, 42, "the caller's variable observes a type-matching write";
}

subtest 'untyped sigilless bind is unaffected (no false-positive check)' => {
    plan 2;
    my $a = 5;
    my \x := $a;
    x = "now a string";
    is $a, "now a string", 'untyped alias write still succeeds';
    is x, "now a string", 'untyped alias reads the new value';
}
