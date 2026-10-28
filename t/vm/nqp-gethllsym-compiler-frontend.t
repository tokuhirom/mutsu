use Test;
use nqp;

# Distributions branch on the compiler frontend (Red's t/88-projection-chains
# marks its interpolation subtests `todo` when it is 'rakuast'); Rakudo's
# default frontend, and mutsu's (ADR-10723), is RakuAST.

plan 4;

my $frontend = nqp::gethllsym('Raku', 'COMPILER-FRONTEND');
is $frontend, 'rakuast', 'the Raku HLL names its frontend';
is nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), 'legacy'), 'rakuast',
    'nqp::ifnull keeps the bound value';
ok nqp::isnull(nqp::gethllsym('Raku', 'NO-SUCH-SYMBOL')) || !nqp::gethllsym('Raku', 'NO-SUCH-SYMBOL').defined,
    'an unbound symbol is null';

nqp::bindhllsym('Raku', 'MY-TEST-SYMBOL', 'bound');
is nqp::gethllsym('Raku', 'MY-TEST-SYMBOL'), 'bound', 'bindhllsym round-trips';
