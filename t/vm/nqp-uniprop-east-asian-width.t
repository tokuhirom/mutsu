use v6.d;
use Test;
use nqp;

# From Text::MiscUtils (Layout.rakumod): it takes nqp::unipropcode('East_Asian_Width')
# and reads each codepoint's width class with getuniprop_str / getuniprop_bool.
# The numbers are MoarVM's, read off rakudo.

plan 14;

my $p = nqp::unipropcode('East_Asian_Width');
is $p, 7, 'property code';
is nqp::unipropcode('ea'), 7, 'short name';

is nqp::unipvalcode($p, 'W'), 3, 'value code of W';
is nqp::unipvalcode($p, 'Na'), 5, 'value code of Na';
is nqp::unipvalcode($p, 'Fullwidth'), 4, 'long value name';
is nqp::unipvalcode($p, 'Zzz'), 0, 'unknown value is 0';

is nqp::getuniprop_str(0x41, $p), 'Na', 'ASCII letter is narrow';
is nqp::getuniprop_str(0x4E00, $p), 'W', 'CJK ideograph is wide';
is nqp::getuniprop_str(0xFF21, $p), 'F', 'fullwidth Latin letter';
is nqp::getuniprop_str(0xFF61, $p), 'H', 'halfwidth punctuation';
is nqp::getuniprop_str(0xA1, $p), 'A', 'ambiguous';

is nqp::getuniprop_int(0x4E00, $p), 3, 'integer value';
ok nqp::matchuniprop(0x4E00, $p, nqp::unipvalcode($p, 'W')), 'matchuniprop';
is nqp::getuniprop_bool(0x41, $p), 1, 'bool is non-zero (Na is not 0)';

done-testing;
