use Test;
use nqp;

# HTML::Strip's HTML::Entity::Fast dependency uses these NQP primitives when
# decoding numeric entities.
plan 8;

is nqp::eqatic('xX', 'X', 1), 1,
    'nqp::eqatic compares a character at an offset without case';
is nqp::eqatic('xy', 'X', 1), 0,
    'nqp::eqatic rejects a different character at an offset';

my $decimal := nqp::radix(10, '060;', 0, 0);
is nqp::atpos($decimal, 0), 60, 'nqp::radix parses decimal digits';
is nqp::atpos($decimal, 1), 3, 'nqp::radix reports significant digit count';
is nqp::atpos($decimal, 2), 3, 'nqp::radix reports the consumed offset';

my $hex := nqp::radix(16, '3c;', 0, 0);
is nqp::atpos($hex, 0), 60, 'nqp::radix parses hexadecimal digits';

my $signed := nqp::radix(10, '-12_30', 0, 6);
is nqp::atpos($signed, 0), -123,
    'nqp::radix parses signs, underscores, and trailing zero flags';
is nqp::atpos($signed, 2), 6,
    'nqp::radix consumes the sign, digits, and underscore';
