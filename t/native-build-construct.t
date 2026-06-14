use v6;
use Test;

# VM-native default construction now covers classes with a `BUILD` submethod
# (Track A ③ constructor). The instance is assembled natively (defaults first,
# matching the interpreter which also applies defaults before BUILD), then the
# BUILD phase runs via the shared `run_build_phase` helper — same ordering /
# dispatch / `fail` semantics as the full constructor. A custom BUILDALL/new
# still falls through, as does a positional argument (`.new` is named-only).

plan 17;

# --- BUILD assigns from a named arg, with a default ---
class P {
    has $.a;
    has $.b;
    submethod BUILD(:$a = 1) { $!a = $a; $!b = $a + 100 }
}
is P.new(a => 5).a, 5, 'BUILD binds a provided named arg';
is P.new(a => 5).b, 105, 'BUILD derives another attribute';
is P.new.a, 1, 'BUILD uses its parameter default when arg omitted';
is P.new.b, 101, 'BUILD-derived attribute with the default';

# --- defaults are applied before BUILD; BUILD overrides ---
class Y { has $.c = 99; submethod BUILD { $!c = 7 } }
is Y.new.c, 7, 'BUILD overrides an attribute default';

# --- an attribute BUILD does not touch keeps its default ---
class DK { has $.a; has $.b = "kept"; submethod BUILD(:$a) { $!a = $a } }
is DK.new(a => 1).b, "kept", 'a default BUILD does not touch is preserved';

# --- `fail` inside BUILD yields a Failure, not a thrown error ---
class F { has $.x; submethod BUILD(:$x) { fail "bad" if $x < 0; $!x = $x } }
ok (try F.new(x => 5)).defined, 'BUILD succeeds for a valid value';
my $r = F.new(x => -1);
ok $r ~~ Failure, 'a `fail` inside BUILD returns a Failure';
nok $r.defined, 'the returned Failure is undefined';

# --- BUILD then TWEAK, in that order ---
class BT {
    has $.a;
    has $.b;
    submethod BUILD(:$a) { $!a = $a }
    submethod TWEAK { $!b = $!a * 10 }
}
is BT.new(a => 3).b, 30, 'BUILD runs before TWEAK';

# --- inheritance: base BUILD then child BUILD, base-first ---
class Base2 { has $.tag; submethod BUILD { $!tag = "B" } }
class Der is Base2 { submethod BUILD { } }
is Der.new.tag, "B", 'base-class BUILD runs for a derived class';

# --- a positional argument to .new is rejected (named-only constructor) ---
class S { has $.x; }
nok (try S.new("pos")).defined, 'a positional arg to .new is rejected';
class Foo9 {
    has $.a;
    has $.b;
    submethod BUILD($foo, :$bar) { $!a = $foo; $!b = $bar }
}
dies-ok { Foo9.new('pos', bar => 'd') },
    'a positional to .new dies even when BUILD has a positional param';

# --- named-only construction is unaffected ---
class N2 { has $.v; submethod BUILD(:$v = 99) { $!v = $v } }
is N2.new.v, 99, 'BUILD parameter default when omitted';
is N2.new(v => 7).v, 7, 'BUILD parameter from a provided named arg';

# --- a custom BUILD suppresses default named-arg -> attribute binding ---
class Keep { has $.x = 42; submethod BUILD() { } }
is Keep.new.x, 42, 'BUILD class keeps the attribute default';
is Keep.new(x => 666).x, 42,
    'explicitly setting an attr is ignored when BUILD does not bind it';
