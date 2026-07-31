use Test;

# A role's non-declaration body statements run once per *composition* — a
# `does`, a pun, or a value-level `but`/`does` mixin — and never at the
# declaration itself. Assertions here are written so that Rakudo passes them
# too, which rules out counting compositions that Rakudo performs at compile
# time (`class C does R { }` on a literal); those are exercised through the
# pun and the runtime-value mixin instead.

plan 16;

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
