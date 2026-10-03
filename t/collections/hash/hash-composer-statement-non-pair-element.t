use v6;
use Test;

# A brace whose first element is a pair composes a Hash even when a later
# element is not a pair (a conditional slip of pairs, say) -- also as a
# statement, e.g. a routine's last statement (MCP's `initialize` response).

plan 5;

my $i = 'x';

sub with-cond($flag) {
    { a => 1, b => 2, ($flag ?? (c => $flag) !! Empty), }
}
isa-ok with-cond($i), Hash, 'a routine whose last statement is such a composer returns a Hash';
is-deeply with-cond($i), {a => 1, b => 2, c => 'x'}, 'the conditional pair is included';
is-deeply with-cond(''), {a => 1, b => 2}, 'and left out when Empty';

isa-ok { a => 1, ($i ?? (c => $i) !! Empty) }, Hash, 'the same brace as a statement-leading term';
my $r = { say-nothing() };
sub say-nothing() { 42 }
isa-ok $r, Block, 'a brace without a leading pair is still a block';
