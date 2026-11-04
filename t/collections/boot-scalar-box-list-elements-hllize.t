use v6;
use Test;
use nqp;

# A raw BOOT scalar box that becomes an element of a list or array literal is
# its high-level value, as in rakudo (#12881). Scalar assignment keeps it raw.

plan 6;

my $b := nqp::box_s("x", nqp::bootstr());
my $i := nqp::box_i(3, nqp::bootint());

is ($b,)[0].^name, "Str", 'List literal element (Str)';
is ($i, $b).map(*.^name).join(" "), "Int Str", 'List literal elements through map';
is [$b][0].^name, "Str", 'Array literal element';
is [$i, $b].map(*.^name).join(" "), "Int Str", 'Array literal elements';
my $x = $b;
is $x.^name, "BOOTStr", 'scalar assignment keeps the raw box';
is $b.^name, "BOOTStr", 'the box itself is unchanged';
