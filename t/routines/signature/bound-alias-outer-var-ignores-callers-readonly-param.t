use v6;
use Test;

plan 6;

# A sub writes an outer `my $alias := $src`. The alias is bound to `$src`'s
# Scalar, so that container decides whether the write is allowed, not a
# same-named readonly parameter of whoever called the sub (#11539, ADR-11142).

my $src = 10;
my $alias := $src;
sub bump-alias { $alias++ }
sub shadow-alias($alias) { bump-alias() }
shadow-alias(0);
is $src, 11, '`++` through a bound alias reaches the source';

my $s2 = 10;
my $a2 := $s2;
sub set-a2 { $a2 = $a2 + 5 }
sub shadow-a2($a2) { set-a2() }
shadow-a2(0);
is $s2, 15, 'assignment through a bound alias reaches the source';

# A chain of aliases shares the one container.
my $x = 1;
my $y := $x;
my $z := $y;
sub bump-z { $z++ }
sub shadow-z($z) { bump-z() }
shadow-z(0);
is $x, 2, 'an alias of an alias is writable too';

# An alias of a readonly parameter stays readonly.
sub f($p) { my $a := $p; sub g { $a = 5 }; g() }
throws-like { f(1) }, Exception, message => /readonly/,
    'assigning through an alias of a readonly parameter still dies';
sub f2($p) { my $a := $p; sub g2 { $a++ }; g2() }
throws-like { f2(1) }, X::Multi::NoMatch,
    '`++` through an alias of a readonly parameter still dies';

# A binding to a bare value is immutable whatever the caller holds (#11192).
my $imm := 42;
sub bump-imm { $imm++ }
sub shadow-imm($imm) { bump-imm() }
throws-like { shadow-imm(0) }, X::Multi::NoMatch, '`++` on a name bound to a value still dies';
