use Test;

# A class-body `:=` declaration binds the declaring scope's variable, so the
# body-scoped name and the outer variable are one container (#10682). mutsu
# used to snapshot the value at class-declaration time: a method read the old
# value after the outer variable changed, while a write through the alias
# still reached the outer variable (computed from the stale read).

plan 10;

# Sigilless and sigiled spellings, file scope.
my $y = 1;
class SigillessBind {
    my \x := $y;
    method m { x }
    method bump { x = x + 1 }
}
$y = 7;
is SigillessBind.new.m, 7, 'sigilless class-body bind reads the current outer value';
SigillessBind.new.bump;
is $y, 8, 'a write through the sigilless alias updates the outer variable';
is SigillessBind.new.m, 8, '... and the alias sees it';

my $z = 1;
class SigiledBind {
    my $w := $z;
    method m { $w }
    method set($v) { $w = $v }
}
$z = 7;
is SigiledBind.new.m, 7, 'sigiled class-body bind reads the current outer value';
SigiledBind.new.set(11);
is $z, 11, 'a write through the sigiled alias updates the outer variable';

# Declared inside a routine.
sub in-routine {
    my $q = 1;
    my class InRoutine {
        my \x := $q;
        method m { x }
        method bump { x = x + 1 }
    }
    $q = 9;
    my @r = InRoutine.new.m;
    InRoutine.new.bump;
    @r.push: $q;
    @r
}
is-deeply in-routine(), [9, 10], 'a routine-scoped class bind aliases the routine lexical';

# The bind identity holds inside the body too.
my $s = 1;
class IdentityInBody {
    my $t := $s;
    our $same = $t =:= $s;
}
ok $IdentityInBody::same, 'the bound name is the same container as the source';

# A block-final bind in an expression block still aliases its source.
my $u = 1;
my $alias-reader = do { my $v := $u; -> { $v } };
$u = 5;
is $alias-reader(), 5, 'block-final `my $v := $u` aliases the source';

# Several binds to the same outer variable share one container.
my $n = 1;
class TwoBinds {
    my $a := $n;
    my \b := $n;
    method sum { $a + b }
}
$n = 3;
is TwoBinds.new.sum, 6, 'two class-body binds of one variable both follow it';

# A class-body static that is not a bind keeps its own value.
my $o = 1;
class NotABind {
    my $copy = $o;
    method m { $copy }
}
$o = 2;
is NotABind.new.m, 1, 'a plain class-body `my $x = $outer` stays a copy';
