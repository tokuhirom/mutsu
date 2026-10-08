use Test;

plan 6;

# A plain `(...)` parameter is one positional: a call-site named argument
# must not be bound as its destructure target (#11891).
dies-ok { EVAL 'sub p(($x)) { $x }; p(a => 1)' }, 'sub-signature param rejects a named argument';
dies-ok { EVAL 'sub p(Pair (:key($k), :value($v))) { "$k=$v" }; p(a => 1)' },
    'typed destructuring param rejects a named argument';
dies-ok { EVAL 'sub p([$x]) { $x }; p(a => 1)' }, 'bracket form rejects a named argument';

# Positional Pairs still destructure.
sub q(Pair (:key($k), :value($v))) { "$k=$v" }
is q((a => 1)), 'a=1', 'parenthesised pair is positional';
my %h = a => 1;
is %h.map(-> (:$key, :$value) { "$key=$value" }).join(','), 'a=1', 'hash iteration pair destructures';
sub r(($x, $y)) { "$x$y" }
is r((1, 2)), '12', 'list destructures';
