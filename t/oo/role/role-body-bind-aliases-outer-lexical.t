use Test;

# A role-body `:=` declaration binds the declaring scope's variable, so the
# body-scoped name, the outer variable and the methods composed from the role
# share one container (#11087). The role body is deferred to composition, and
# mutsu used to bind a snapshot of the value there: a composed method read the
# old value after the outer variable changed, and a write through the alias
# never reached the outer variable.

plan 8;

my $z = 1;
role SigiledBind {
    my $w := $z;
    method m { $w }
    method set($v) { $w = $v }
}
class SigiledC does SigiledBind {}
$z = 7;
is SigiledC.new.m, 7, 'sigiled role-body bind reads the current outer value';
SigiledC.new.set(11);
is $z, 11, 'a write through the sigiled alias updates the outer variable';
is SigiledC.new.m, 11, '... and the alias sees it';

my $y = 1;
role SigillessBind {
    my \x := $y;
    method m { x }
    method bump { x = x + 1 }
}
class SigillessC does SigillessBind {}
$y = 7;
is SigillessC.new.m, 7, 'sigilless role-body bind reads the current outer value';
SigillessC.new.bump;
is $y, 8, 'a write through the sigilless alias updates the outer variable';

# A parametric role runs its body once per composition; every composition
# binds the same outer variable.
my $p = 1;
role ParamBind[::T] {
    my $w := $p;
    method m { $w }
}
class ParamInt does ParamBind[Int] {}
class ParamStr does ParamBind[Str] {}
$p = 3;
is ParamInt.new.m, 3, 'parametric role composition 1 sees the current value';
is ParamStr.new.m, 3, 'parametric role composition 2 sees the current value';

# Composed from inside a routine's frame, not the role's declaring frame.
my $s = 1;
role ComposedElsewhere {
    my $w := $s;
    method m { $w }
}
sub compose-in-routine { my class Inner does ComposedElsewhere {}; Inner.new }
my $obj = compose-in-routine();
$s = 4;
is $obj.m, 4, 'a role composed in another frame still aliases the outer variable';

