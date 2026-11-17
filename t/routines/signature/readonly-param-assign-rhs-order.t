use Test;

# An assignment evaluates its right-hand side before it stores, so an
# assignment to a readonly target runs the RHS and then dies, as in rakudo.
# `CheckReadOnly` sits right before the store it guards, which is also what
# lets its "assign through the bound object" verdict be a mark that store
# consumes (ADR-10779 D3). Outputs checked against rakudo.

plan 6;

my @log;
sub f($x) { $x = do { @log.push('rhs'); 1 } }
dies-ok { f(1) }, 'assigning to a readonly parameter dies';
is-deeply @log, ['rhs'], '... after its RHS ran';

@log = ();
sub g($x) { $x ~= do { @log.push('rhs'); 'a' } }
try g('s');
is-deeply @log, ['rhs'], 'a fused ~= runs its RHS before the readonly check too';

class FI { has $.v is rw; method STORE($n) { $!v = $n } }
my \x = FI.new(v => 1);
my $y = 2;
$y = 3;
x = 7;
is x.v, 7, 'a sigilless term bound to an object with STORE assigns through it';
is $y, 3, 'a plain assignment before it is unaffected';

my \a = [1, 2];
a = 3, 4;
is-deeply a, [3, 4], 'a sigilless term bound to an Array assigns into it';
