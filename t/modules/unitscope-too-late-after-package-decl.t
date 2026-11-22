use Test;

# A `unit` declarator after the compilation unit has already declared a
# package-like type is X::UnitScope::TooLate, as on rakudo 2026.11 (#18588):
# "Too late for unit-scoped module definition; Please use the block form."
# The earlier package may be a class, role, grammar, module or package
# block, at any depth; an enum, a subset, a routine or a plain statement
# before the `unit` declarator is fine.

plan 11;

for ｢role R { }; unit module M;｣                => 'module',
    ｢my class C { }; unit module M;｣            => 'module',
    ｢module X { }; unit class C;｣               => 'class',
    ｢grammar G { }; unit role R;｣               => 'role',
    ｢{ class Inner { } }; unit module M;｣       => 'module',
    ｢class A { }; unit sub MAIN();｣             => 'sub' -> (:key($code), :value($what)) {
    throws-like $code, X::UnitScope::TooLate, :$what,
        message => "Too late for unit-scoped $what definition;\nPlease use the block form.",
        "too late: $code";
}

for ｢sub f { }; unit module M1;｣,
    ｢enum E <a b>; unit module M2;｣,
    ｢subset S of Int; unit module M3;｣,
    ｢my $x = 1; unit class C4;｣,
    ｢BEGIN { }; unit class C5;｣ -> $code {
    lives-ok { EVAL $code }, "allowed: $code";
}
