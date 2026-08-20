use Test;

# A role's non-declaration body statements run once per *composition* — a
# `does`, a pun, or a value-level `but`/`does` mixin — and never at the
# declaration itself. Assertions here are written so that Rakudo passes them
# too, which rules out counting compositions that Rakudo performs at compile
# time (`class C does R { }` on a literal); those are exercised through the
# pun and the runtime-value mixin instead.

plan 23;

# --- declaration runs nothing --------------------------------------------
my $decl-ran = 0;
role Silent { $decl-ran++ }
is $decl-ran, 0, 'role body does not run at declaration';

# --- punning is a composition --------------------------------------------
my $punned = 0;
role Punny { $punned++; method p { 'p' } }
is $punned, 0, 'pun role body has not run yet';
my $obj = Punny.new;
is $punned, 1, 'punning the role ran its body';
is $obj.p, 'p', 'the pun carries the role method';
Punny.new;
is $punned, 1, 'a second pun is memoised';

# --- mixing a role into a value is a composition --------------------------
my $mixed = 0;
role Mixy2 { $mixed++; method m { 'm' } }
sub mix($n) { $n but Mixy2 }
my $a = mix(1);
is $mixed, 1, 'value mixin runs the role body';
mix(2);
is $mixed, 1, 'a second mixin of the same role is memoised';
is $a.m, 'm', 'the mixin carries the role method';

# --- composing a role composes the roles it composes ----------------------
# Their bodies run too, nearest first. The order is accumulated through a
# package variable rather than a lexical: Rakudo clones a role body's closure
# per composition, so a write to an outer `my` from the composed-into role's
# body does not reach the declaring frame's container.
our $order = '';
role Grand { $GLOBAL::order ~= 'G' }
role Middle does Grand { $GLOBAL::order ~= 'M'; method mm { 'mm' } }
is $GLOBAL::order, '', 'a composed-into role body has not run yet';
Middle.new;
is $GLOBAL::order, 'MG',
    'punning runs the role body, then its composed roles\', nearest first';

# --- a guard in a parameterised role body rejects the parameterisation ----
class Plain { }
role Guarded[::T] { die "not allowed" if T ~~ Plain; method g { 'g' } }
my $ok = 5 but Guarded[Int];
is $ok.g, 'g', 'a passing parameterisation composes onto a value';
dies-ok { my $bad = 5 but Guarded[Plain] },
    'role body guard rejects a bad parameterisation on the mixin path';

# --- `also does` inside a class body is a composition too -----------------
# Not pre-initialised: Rakudo composes this at compile time, so a runtime
# `our $also = ''` would clobber what the body wrote.
role Also { $GLOBAL::also = 'X'; method x { 'x' } }
class WithAlso { also does Also; }
is $GLOBAL::also, 'X', '`also does` runs the role body';

# --- a role composed into a grammar --------------------------------------
role Method-Role { method hi { 'HI' } }
grammar WithMethod does Method-Role { token TOP { 'x' } }
is WithMethod.hi, 'HI', 'a grammar composes its `does` role';
ok ?WithMethod.parse('x'), 'the grammar still inherits Grammar';

# --- a role's token belongs to the composing grammar, not to the world ----
role RoleA { token item { 'a' } }
role RoleB { token item { 'b' } }
grammar GA does RoleA { token TOP { <item> } }
grammar GB does RoleB { token TOP { <item> } }
ok ?GA.parse('a') && !GA.parse('b') && ?GB.parse('b') && !GB.parse('a'),
    'each grammar sees only its own role\'s token';

# --- the memo key covers class composition and value/instance mixin too ---
# (todo/deep/adr0019-role-composition-memo-guard-raku-case-table.md's case
# table, verified against `raku`.)

# Two distinct classes composing the same role are not memoised across
# classes -- each is its own composition and runs the body once.
# `class X does R {}` composes at compile time in real Raku, so this can't
# be observed via `my $x = 0; ...; is $x, ...`: a runtime reset/read always
# runs AFTER compile-time composition already happened. Accumulate into
# $GLOBAL:: without an intervening reset instead, like $order above.
role TwoClassRole { $GLOBAL::two_class_count++; method t { 't' } }
class TwoClassA does TwoClassRole { }
class TwoClassB does TwoClassRole { }
is $GLOBAL::two_class_count, 2,
    'two distinct classes composing the same role each run its body once';

# Redeclaring the very same class (by name) does not re-run the role body a
# second time, even from inside a runtime loop that re-executes the
# `class ... does ...` statement on every pass -- only role composition is
# idempotent, not the class's own mainline, which still runs every pass.
role LoopRole { $GLOBAL::loop_role_count++; method l { 'l' } }
my $loop-body-count = 0;
for 1..3 {
    class LoopClass does LoopRole {
        $loop-body-count++;
    }
}
is $GLOBAL::loop_role_count, 1,
    're-declaring the same class in a loop composes the role body only once';
is $loop-body-count, 3,
    q[but the class's own mainline still runs every pass];

# `but`/`does` onto two DIFFERENT base types is not memoised across types:
# Rakudo's memoized composed type is `Int+{R}` vs `Str+{R}` -- two distinct
# anonymous types -- so each gets its own run. (A bare, method-less role
# composed via a LITERAL value is itself constant-folded and composed at
# compile time same as the class-header case above, so the values must come
# from a sub parameter, and the role needs a method, to force genuine
# runtime composition on every `raku` implementation.)
my $mixed-count = 0;
role BaseTypeRole { $mixed-count++; method b { 'b' } }
sub mix-val($n) { $n but BaseTypeRole }
mix-val(1);
mix-val("x");
is $mixed-count, 2,
    'but onto two different base types runs the role body for each type';

# `$obj does R` reblesses the object into a synthesized `C+{R}` class via the
# same class-composition machinery as a literal `class C does R {}`, so it
# needs $GLOBAL:: (not a lexical closure) to observe the run count reliably,
# same as the class-header cases above.
role DoesSingle { $GLOBAL::does_single++; method d { 'd' } }
class DoesSingleTarget { }
my $dso = DoesSingleTarget.new;
$dso does DoesSingle;
is $GLOBAL::does_single, 1,
    'a single runtime `does` on an instance runs the role body once';

# Two instances of the SAME class both `does`-ing the same role share the
# one synthesized mixin type, so the body runs once, not twice.
role DoesSame { $GLOBAL::does_same++; method d { 'd' } }
class DoesSameTarget { }
my $d1 = DoesSameTarget.new;
my $d2 = DoesSameTarget.new;
$d1 does DoesSame;
$d2 does DoesSame;
is $GLOBAL::does_same, 1,
    'runtime `does` on two instances of the same class is memoised';

# Instances of two DIFFERENT classes `does`-ing the same role each
# synthesize their own distinct mixin type, so each runs the body once.
role DoesDiff { $GLOBAL::does_diff++; method d { 'd' } }
class DoesDiffA { }
class DoesDiffB { }
my $da = DoesDiffA.new;
my $db = DoesDiffB.new;
$da does DoesDiff;
$db does DoesDiff;
is $GLOBAL::does_diff, 2,
    'runtime `does` on instances of two different classes runs the role body for each';
