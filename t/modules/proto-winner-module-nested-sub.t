use lib 't/lib';
use Test;

# A trivial proto's winner, defined in a module and reached through an
# exported wrapper, calls its own `my sub`: the call must resolve that sub in
# the module's nested-sub table, not the importing script's (it died with
# "frame-lexical routine 'compare' has no compiled body"; Algorithm::Diff).

use ProtoWinnerNestedSub;

plan 6;

ok is-same(1, 1), 'winner calls its nested sub';
nok is-same(1, 2), 'and again on a second call';
is common-prefix(<a b c>, <a b x>), 2, 'a winner with a defaulted Callable parameter';
is common-prefix(<a b c>, <a b x>), 2, 'repeated';
is common-prefix-fc(<A b c>, <a B x>), 2, 'with a Callable argument captured by the nested sub';
is by-hash(['a']), 'hash', 'the other candidate still wins on its arguments';
