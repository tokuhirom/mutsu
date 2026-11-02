use Test;

plan 4;

# A fresh `my $*X` in a nested block is a new dynamic: it must not write through
# the caller's `$*X` that this routine bound earlier (`my $h := $*X`).
sub inner() { my $h := $*HI; { my $*HI = 7; is $*HI, 7, 'the inner dynamic holds its own value' }; $h }
sub outer() { my $*HI = 3; inner() }
is outer(), 3, 'the caller binding keeps its value';
is $*HI.defined, False, 'and nothing leaked to the top level';

sub inner2() { my $h := $*H2; { my $*H2 := Str; }; 'ok' }
sub outer2() { my $*H2 := Str; inner2() }
is outer2(), 'ok', 'declaring over a bound caller dynamic does not die';
