use MONKEY-SEE-NO-EVAL;
use Test;

# A `my` inside EVAL'd code is scoped to that EVAL. When the caller happens to
# use the same name the declaration SHADOWS it, so the caller's variable must be
# untouched — including when the snippet then throws, which is the shape
# `throws-like 'my $x = ...; die ...'` produces.

plan 10;

my $a = 10;
EVAL 'my $a = 999';
is $a, 10, 'EVAL my does not clobber a same-named caller scalar';

my $b = 10;
try { EVAL 'my $b = 999; die "boom"' };
is $b, 10, 'and not when the snippet dies either';

my $c = 10;
try { EVAL 'my $c = 999; my $extra = 1; die "boom"' };
is $c, 10, 'a second declaration in the snippet changes nothing';

# A plain assignment still writes through — that is not a declaration.
my $d = 10;
EVAL '$d = 999';
is $d, 999, 'EVAL assignment to a caller scalar still writes through';

my $e = 10;
try { EVAL '$e = 999; die "boom"' };
is $e, 999, 'partial work before a throw survives';

# Containers take the same rule.
my @arr = 1, 2;
try { EVAL 'my @arr = 7, 8, 9; die "boom"' };
is-deeply @arr, [1, 2], 'EVAL my does not clobber a same-named caller array';

my %h = a => 1;
try { EVAL 'my %h = b => 2; die "boom"' };
is-deeply %h, {a => 1}, 'EVAL my does not clobber a same-named caller hash';

# The declaration is still visible *inside* the EVAL, and is its value.
is EVAL('my $f = 42; $f'), 42, 'the EVAL sees its own declaration';

# It must not leak as a new name either (the pre-existing rule).
my $g = 10;
{
    my $s = sub { try { EVAL 'my $g = 999; die "boom"' } };
    $s();
}
is $g, 10, 'a closure frame between the caller and the EVAL changes nothing';

sub via-routine($code) { try { EVAL $code } }
my $h = 10;
via-routine('my $h = 999; die "boom"');
is $h, 10, 'a routine frame between the caller and the EVAL changes nothing';
