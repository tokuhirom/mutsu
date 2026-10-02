use Test;

# The compile-time checks an EVAL'd snippet goes through judge a name in
# every position rakudo judges it -- a routine or method body, a closure, a
# condition, a parameter -- not only at the snippet's top level. Each walk is
# the typed AST visitor (ADR-0137). Every expectation below was checked
# against rakudo.

plan 30;

# Undeclared routines: the mainline CHECK-time analysis, in its EVAL mode.
throws-like { EVAL 'class C1 { has $.foo; method m { foo() } }' }, X::Undeclared::Symbols,
    'an attribute does not declare a routine called from a method';
throws-like { EVAL 'my $c = -> { zork() }' }, X::Undeclared::Symbols,
    'an undeclared routine called in a pointy block';
lives-ok { EVAL 'sub outer1 { inner1() }; sub inner1 { 1 }' },
    'a routine declared later in the unit';

# Undeclared bareword names.
throws-like { EVAL 'sub f2 { Nope }' }, X::Undeclared::Symbols,
    'an undeclared name in a sub body';
throws-like { EVAL 'for 1 { Nope }' }, X::Undeclared::Symbols,
    'an undeclared name in a loop body';
lives-ok { EVAL 'sub f3(\x) { x }; f3(1)' }, 'a sigilless parameter';
lives-ok { EVAL 'sub f4 { constant Z4 = 1; Z4 }; f4()' }, 'a constant declared in the sub';
is EVAL('constant C5 = 5; C5'), 5, 'a constant used at the top level';
is EVAL('my \sl6 = 6; sl6'), 6, 'a sigilless variable used at the top level';

# Illegally post-declared types.
throws-like { EVAL 'sub f7 { P7.new }; class P7 { }' }, X::Undeclared::Symbols,
    'a type used in a sub body before its declaration';
throws-like { EVAL 'class A8 { method m { B8.new } }; class B8 { }' }, X::Undeclared::Symbols,
    'a type used in a method body before its declaration';

# BEGIN sees only the routines declared before it.
throws-like { EVAL 'BEGIN { if True { later9() } }; sub later9 { }' }, X::Undeclared::Symbols,
    'a nested call in BEGIN to a routine declared afterwards';

# `our sub` installs one package symbol, wherever it is declared.
throws-like { EVAL 'sub f10 { our sub g10 { } }; our sub g10 { }' }, X::Redeclaration,
    'an our sub redeclared after one in a sub body';

# Undeclared variables.
throws-like { EVAL 'sub f11 { $nope11 }' }, X::Undeclared, 'a variable in a sub body';
throws-like { EVAL 'my @barf12 = 1, 2; say $barf12[1]' }, X::Undeclared,
    'an array does not declare the scalar of the same name';
throws-like { EVAL 'sub f15 { my $r; $r = $foo15 ~ my $foo15 }' }, X::Undeclared,
    'a use before an embedded declaration';
{
    # A lexical the caller declares later in its source is in its pad
    # already; the EVAL check cannot see it, so it does not judge an operand.
    sub e13($s) { EVAL $s }
    ok e13('!$y13.defined'), 'a later caller lexical in an operand';
    my $y13 = 4;
}
lives-ok { EVAL 'my $c14 = -> $a { $a + 1 }; $c14(1)' }, 'a pointy block parameter';
lives-ok { EVAL 'sub f16 { @_ }; sub g16 { %_ }; my $c = { @_ }' },
    'the implicit @_ and %_ of a routine or block';
lives-ok { EVAL 'loop (my $i17 = 0; $i17 < 1; $i17++) { }; $i17' },
    'a loop header declares into the enclosing scope';
lives-ok { EVAL 'for 1, 2 -> $a18, $b18 { $a18 + $b18 }' }, 'loop parameters';
is EVAL('my $x18 = 7 for ^1; $x18'), 7, 'a statement modifier opens no scope';
lives-ok { EVAL 'my $x19 = 1; sub f19 { my $l = $x19; { $l } }' },
    'outer and enclosing-block lexicals';
is EVAL('enum E20 <A20 B20>; A20'), 'A20', 'an enum value used at the top level';

# Type arguments, parameter types and inheritance from a type capture.
lives-ok { EVAL 'role R21[::T] { method m { my Array[T] $x } }' },
    'a role type parameter as a type argument';
lives-ok { EVAL 'sub f22(::T $a, Array[T] $b) { }' },
    'a signature capture as a type argument';
throws-like { EVAL 'class Q23 { method m(Array[Numerix] $x) { } }' }, X::Undeclared::Symbols,
    'an undeclared type argument of a method parameter';
throws-like { EVAL 'if True { sub f24(Junctoin $x) { } }' }, X::Parameter::InvalidType,
    'an invalid parameter type of a sub in a block';
lives-ok { EVAL 'my $c = -> ::T $x { sub f25(T $y) { } }' },
    'a block signature capture as a parameter type';
throws-like { EVAL 'role R26[::T] { my class C26 is T { } }' }, X::Inheritance::Unsupported,
    'inheriting from a role type parameter';
