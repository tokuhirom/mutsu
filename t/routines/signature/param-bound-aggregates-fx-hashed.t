use Test;
plan 6;

# `@`/`%` parameter bindings are recorded per name and per container address
# (ParamBoundAggregates); the table is Fx-hashed now (#15688). Two closures made
# by one routine, each capturing its `%g` bound to a different caller hash, must
# keep their own bindings when threads start from them (#10076).
sub mk(%g) { -> { %g<k> } }
my %a = k => 1;
my %b = k => 2;
my $fa = mk(%a);
my $fb = mk(%b);
is $fa(), 1, 'the first closure sees its own hash';
is $fb(), 2, 'the second closure sees its own hash';

sub bump(@x) { await start { @x.push(9); @x.elems } }
my @l = 1, 2;
is bump(@l), 3, 'a started block sees the bound array';
is @l.elems, 3, 'and its push is visible to the caller';

sub total(%h) { [+] await (1..3).map: -> $i { start { %h.values.sum + $i } } }
is total({ a => 1, b => 2 }), 15, 'blocks started in a map over a bound hash';
is (|%a, |%b).elems, 2, 'unrelated hashes are untouched';
