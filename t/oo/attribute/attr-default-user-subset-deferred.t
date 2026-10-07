use Test;

# From RPi::Device::PiGlow: an attribute default checked against a user
# `subset` is a construction-time check, not a declaration-time error.

plan 5;

subset DevPath of Str where { $_.IO ~~ :e };
class Foo {
    has DevPath $.p = '/nonexistent/zzz-mutsu';
}
pass "class with failing subset default declares";
my $f = try Foo.new;
ok !$f.defined, "construction fails the where clause";
isa-ok $!, X::TypeCheck::Assignment, "as an assignment type-check error";
is Foo.new(p => $*PROGRAM.Str).p, $*PROGRAM.Str, "explicit value passes";

subset S of Int;
class B { has S $.p = "x"; }
pass "plain subset with wrong-typed default declares";
