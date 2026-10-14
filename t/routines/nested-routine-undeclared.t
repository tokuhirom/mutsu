use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# A routine declared inside another routine's body is lexically visible inside
# that body only (#13111). An `our` one is also installed as a package symbol,
# but only a qualified call reaches it; a bare call outside the body is
# rakudo's CHECK-time "Undeclared routine":
#
#   sub f { sub helper { 1 } }; say helper()
#     -> ===SORRY!=== Undeclared routine: helper used at line 1
#
# mutsu collected declarations scope-blind, so any `sub helper` anywhere in
# the unit suppressed the error. Types and variables stay scope-blind: a
# `class K` in a routine body is `our`-scoped and reachable from outside.

plan 13;

throws-like 'sub f { sub helper { 1 } }; helper()', X::Undeclared::Symbols,
    'a `my` sub in a routine body is not visible outside it',
    message => /'helper used at line'/;

throws-like 'sub f2 { our sub ourhelper { 1 } }; ourhelper()', X::Undeclared::Symbols,
    'an `our` sub in a routine body is not reachable by a bare call outside it';

throws-like 'class C { method m { sub mhelper { 1 } } }; mhelper()', X::Undeclared::Symbols,
    'a sub in a method body is not visible outside it';

throws-like 'sub f { proto phelper(|) {*}; multi phelper($) { 1 } }; phelper(1)',
    X::Undeclared::Symbols,
    'a proto/multi in a routine body is not visible outside it';

is EVAL('sub f { my $r = helper() ~ helper2(); sub helper { "a" }; sub helper2 { "b" }; $r }; f()'), 'ab',
    'a body routine is visible to calls in the body before its declaration';

is EVAL('sub f { sub g { helper() }; sub helper { 7 }; g() }; f()'), 7,
    'a body routine is visible to a nested routine of that body';

is EVAL('sub f { sub helper { 3 } }; sub helper { 4 }; helper()'), 4,
    'a top-level routine of the same name is still found';

is EVAL('sub f { class K { method v { 5 } } }; K.new.v'), 5,
    'a class declared in a routine body stays reachable outside it';

sub f9 { our sub ghelper { 6 } }
is GLOBAL::ghelper(), 6,
    'an `our` body routine is reachable qualified';

is EVAL('sub fact($n) { $n <= 1 ?? 1 !! $n * fact($n - 1) }; fact(5)'), 120,
    'a routine still sees its own name in its body';

# In a program, an `our` body routine is in the package before the mainline
# runs (#13091); a bare call outside the body still does not reach it.
is_run ｢sub f { our sub helper { 1 } }; say "ran"; say helper()｣,
    %(:out(''), :err(/'Undeclared routine:' \s+ 'helper used'/), :status(1)),
    'a program calling an `our` body routine bare is rejected at compile time';

is_run ｢sub f { sub helper { 1 } }; say "ran"; say helper()｣,
    %(:out(''), :err(/'Undeclared routine:' \s+ 'helper used'/), :status(1)),
    'a program calling a `my` body routine bare is rejected at compile time';

is_run ｢sub f { helper(); sub helper { say "h" } }; f()｣,
    %(:out("h\n"), :err(''), :status(0)),
    'a program calling a body routine inside its body runs';
