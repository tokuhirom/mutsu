use Test;

# `DYNAMIC::<$y>` for a plain lexical must not stop at a routine whose
# nested block (not running) declares its own `my $y`: that slot belongs to
# the block, not to the routine's dynamic scope (#19182). Checked against
# rakudo.
plan 5;

sub r { DYNAMIC::<$y> }
sub f { my $a = r(); { my $y = 2 }; $a }
sub f2 { { my $y = 2 }; r() }
sub f3 { { my $y = 2; { my $y = 9 } }; r() }
sub g { my $a = r(); my $y = 3; $a }
my $y = 1;

is f(), 1, 'a dormant block\'s my $y does not hide the outer $y (call before block)';
is f2(), 1, 'a dormant block\'s my $y does not hide the outer $y (call after block)';
is f3(), 1, 'nested dormant blocks do not hide the outer $y';
is g(), 1, 'a later declaration in the routine does not shadow';
is-deeply (sub { r() })(), 1, 'an anonymous caller sees the outer $y';

done-testing;
