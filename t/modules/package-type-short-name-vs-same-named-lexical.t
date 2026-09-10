use lib 't/lib';
use Test;
use MONKEY-SEE-NO-EVAL;
use PackageTypeShortName;

# A type declared INSIDE a package is registered under its qualified name only
# (`M::C`), so a bareword `C` is a reference that has to be resolved through the
# running package chain. A `my C $C` declaration stores the scalar under the
# SAME sigil-stripped lexical key as that bareword, which used to make the
# bareword resolve to a bare, never-registered `C` type object with no methods —
# so `my C $C .= new(...)` (the fused form calls `.new` on the bareword) died
# with "Unknown method ... new on C". Everything below is the matrix of where
# such a declaration can appear.

plan 35;

# --- mainline, no package involved -------------------------------------------

class Main1 { has $.a }
my Main1 $Main1;
is $Main1.^name, Main1.^name, 'mainline: same-named typed decl keeps the class identity';
is Main1.new(:a(1)).a, 1, 'mainline: bareword .new still works after a same-named decl';

class Main2 { has $.a }
my Main2 $Main2 .= new(:a(2));
is $Main2.a, 2, 'mainline: fused `.= new` on a same-named typed decl';
is $Main2.^name, Main2.^name, 'mainline: fused `.= new` produces the real class';

# Other-name control.
class Main3 { has $.a }
my Main3 $other .= new(:a(3));
is $other.a, 3, 'mainline: other-named typed decl (control)';

# Untyped control.
class Main4 { has $.a }
my $Main4 = Main4.new(:a(4));
is $Main4.a, 4, 'mainline: untyped same-named decl (control)';

# Nested-block control: the declaration is confined to the block.
class Main5 { has $.a }
{ my Main5 $Main5; }
is Main5.new(:a(5)).a, 5, 'mainline: same-named decl in a nested block (control)';

# Builtin control: a same-named lexical must not shadow a core type either.
my Int $Int;
is $Int.^name, 'Int', 'mainline: `my Int $Int` still holds the Int type object';
is Int.^name, 'Int', 'mainline: bareword Int after `my Int $Int`';

# --- inside a plain sub of this compilation unit ------------------------------

sub in-plain-sub() {
    class Sub1 { has $.a }
    my Sub1 $Sub1 .= new(:a(11));
    ($Sub1.a, $Sub1.^name, Sub1.^name)
}
my ($sa, $sn, $sb) = in-plain-sub();
is $sa, 11, 'plain sub: fused `.= new` on a same-named typed decl';
is $sn, $sb, 'plain sub: the value and the bareword name the same type';

# --- inside a sub of a SEPARATE compilation unit (a module) -------------------

is module-scope-decl-name(), module-scope-bareword-name(),
    'module scope: `my Modish $Modish` keeps the class identity';
is module-scope-bareword-name(), 'PackageTypeShortName::Modish',
    'module scope: the bareword resolves to the package-qualified class';

my ($ma, $mn, $mb) = in-module-sub();
is $ma, 7, 'module sub: fused `.= new` on a same-named typed decl';
is $mn, $mb, 'module sub: the value and the bareword name the same type';

# --- EVAL from mainline -------------------------------------------------------

is EVAL('class Ev1 { has $.a }; my Ev1 $Ev1 .= new(:a(21)); $Ev1.a'), 21,
    'EVAL at mainline: fused `.= new` on a same-named typed decl';
is EVAL('class Ev2 { has $.a }; my Ev2 $Ev2; $Ev2.^name eq Ev2.^name'), True,
    'EVAL at mainline: the value and the bareword name the same type';
is EVAL('class Ev3 { has $.a }; my Ev3 $z .= new(:a(23)); $z.a'), 23,
    'EVAL at mainline: other-named typed decl (control)';

# --- EVAL from a plain sub of this compilation unit ---------------------------

sub eval-in-plain-sub($code) { EVAL $code }

is eval-in-plain-sub('class Ep1 { has $.a }; my Ep1 $Ep1 .= new(:a(31)); $Ep1.a'), 31,
    'EVAL from a plain sub: fused `.= new` on a same-named typed decl';
is eval-in-plain-sub('class Ep2 { has $.a }; my Ep2 $Ep2; $Ep2.^name eq Ep2.^name'), True,
    'EVAL from a plain sub: the value and the bareword name the same type';
is eval-in-plain-sub('class Ep3 { has $.a }; my Ep3 $z .= new(:a(33)); $z.a'), 33,
    'EVAL from a plain sub: other-named typed decl (control)';

# --- EVAL from a sub of a SEPARATE compilation unit ---------------------------
# This is the shape the real `Test.rakumod` uses: its `eval_exception` runs the
# snippet from a sub of another compunit, so the snippet's `class` is registered
# under THAT module's package.

is eval-in-module('class Em1 { has $.a }; my Em1 $Em1 .= new(:a(41)); $Em1.a'), 41,
    'EVAL from a module sub: fused `.= new` on a same-named typed decl';
is eval-in-module('class Em2 { has $.a }; my Em2 $Em2; $Em2.^name eq Em2.^name'), True,
    'EVAL from a module sub: the value and the bareword name the same type';
is eval-in-module('class Em3 { has $.a }; my Em3 $z .= new(:a(43)); $z.a'), 43,
    'EVAL from a module sub: other-named typed decl (control)';
is eval-in-module('class Em4 { has $.a }; my $Em4 = Em4.new(:a(44)); $Em4.a'), 44,
    'EVAL from a module sub: untyped same-named decl (control)';
is eval-in-module('class Em5 { has $.a }; { my Em5 $Em5; }; Em5.new(:a(45)).a'), 45,
    'EVAL from a module sub: same-named decl in a nested block (control)';
is eval-in-module('my Int $Int; $Int.^name'), 'Int',
    'EVAL from a module sub: `my Int $Int` still holds the Int type object';

# The class an EVAL declares from a module sub is a member of that module's
# package, and its accessor must survive the same-named declaration.
is eval-in-module('class Em6 { has $.a }; my Em6 $Em6; Em6.^name'),
   eval-in-module('class Em7 { has $.a }; my Em7 $z; Em7.^name').subst('Em7', 'Em6'),
    'EVAL from a module sub: same-named and other-named decls resolve alike';
is eval-in-module('class Em8 { has $.a }; my Em8 $Em8; Em8.new(:a(48)).a'), 48,
    'EVAL from a module sub: bareword .new after a same-named decl';

# Nothing above may leave the snippet dying.
ok !eval-exception('class Em9 { has $.a }; my Em9 $Em9 .= new(:a(9))').defined,
    'EVAL from a module sub: the fused declaration does not throw';

# --- the type object itself ---------------------------------------------------

class Undef1 { has $.a }
my Undef1 $Undef1;
nok $Undef1.defined, 'an uninitialized same-named typed scalar is undefined';
isa-ok $Undef1, Undef1, 'an uninitialized same-named typed scalar IS the declared type';
ok Undef1.^can('a'), 'the class resolved from the bareword has its accessor';

class Undef2 { has $.a }
my Undef2 $Undef2;
is $Undef2.WHAT.^name, Undef2.^name, '.WHAT of the seeded type object names the class';
ok $Undef2.^can('a'), 'the seeded type object carries the class methods';
