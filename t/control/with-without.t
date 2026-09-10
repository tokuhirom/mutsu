use Test;
plan 4;

my $x = 42;
my $y;

my $r1 = "";
with $x { $r1 = "defined"; }
is $r1, "defined", 'with works for defined value';

my $r2 = "undef";
with $y { $r2 = "defined"; }
is $r2, "undef", 'with skips for undefined value';

my $r3 = "";
without $y { $r3 = "undefined"; }
is $r3, "undefined", 'without works for undefined value';

my $r4 = "ok";
without $x { $r4 = "bad"; }
is $r4, "ok", 'without skips for defined value';
