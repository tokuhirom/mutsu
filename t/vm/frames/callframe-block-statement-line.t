use v6;
use Test;

plan 8;

# A block's call frame reports the line of the statement executing in the
# block, not the line the enclosing statement (the one that built the block)
# started on (#13627). Expected values are the `raku` output.

sub who1() { callframe(1).line }
sub who2() { who1x() }
sub who1x() { callframe(2).line }
sub cap(&b) { b() }

my $c = cap {
    my $x = 1;
    my $r = who2();
    $r
};
is $c, 17, 'callframe(2) through a sub reports the statement line in a bare block';

my $d = cap -> {
    my $x = 1;
    my $r = who2();
    $r
};
is $d, 24, 'same for a pointy block';

my $e = cap {
    my $r;
    $r = who2();
    $r
};
is $e, 31, 'an assignment statement after a declaration';

my $f = cap {
    1;
    who1()
};
is $f, 38, 'callframe(1) from a sub called on a later block line';

my &g = {
    my $x = 1;
    who1()
};
is g(), 44, 'a block stored in a variable';

my @lines = (1, 2).map: {
    my $x = $_;
    who1()
};
is @lines, '50 50', 'a map block';

my &k = {
    my $x = 1;
    die "boom";
};
try k();
ok $!.backtrace.map(*.line).grep(56), 'a die in a block reports its own line';

class E { method be { callframe(1).line } }
sub ex { E.new }
my $m = cap {
    ex().be;
};
is $m, 64, 'a method called on the first line of a block body reports that line';
