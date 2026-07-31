use v6;
use Test;

# A subset `where` block written with a placeholder inside a CHAINED
# comparison — `where { 0 <= $^port <= 0xFFFF }`, Cro::Core's Cro::Port —
# died with X::Placeholder::Block: the chained-comparison desugar wraps the
# body in a compiler-generated DoBlock, and its stray-placeholder check did
# not know the enclosing closure's signature (or, on the re-entrant
# block-eval lane, the caller's env binding) had attached the placeholder.

plan 8;

my $f = { 0 <= $^p <= 5 };
ok $f(3), 'chained comparison with placeholder in a plain block (in range)';
nok $f(9), 'chained comparison with placeholder in a plain block (out of range)';

subset Port of Int where { 0 <= $^port <= 0xFFFF };
ok 31313 ~~ Port, 'subset where-block placeholder chain matches in-range';
nok 70000 ~~ Port, 'subset where-block placeholder chain rejects out-of-range';
nok -1 ~~ Port, 'subset where-block placeholder chain rejects negative';

my Port $p = 8080;
is $p, 8080, 'typed declaration through the subset works';

class Listener {
    has Port $.port is required;
}
is Listener.new(port => 31313).port, 31313, 'attribute typed with the subset accepts a valid value';
throws-like { Listener.new(port => 70000) }, Exception,
    'attribute typed with the subset rejects an invalid value';

done-testing;
