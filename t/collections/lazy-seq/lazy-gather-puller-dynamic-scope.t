use v6;
use Test;

# Red (#15112): a lazy gather body runs in the dynamic scope of whoever pulls
# it, not of the call chain that created it.

plan 5;

my @seen;
sub dyn-list() { @seen.push($*DB // 'UNDEF'); (1, 2, 3) }
sub mk() { lazy gather { take $_ for dyn-list() } }
sub run(&b) { b() }

run({ my $*DB = 'mock'; is mk().head(2).Array.raku, '[1, 2]', 'values through a callee-made lazy gather' });
is @seen.join(','), 'mock', 'the body saw the consumer block\'s dynamic variable';

@seen = ();
my $g = mk();
{ my $*DB = 'first'; $g[0] }
is @seen.join(','), 'first', 'the pulling block decides';

@seen = ();
my $h = mk();
{ my $*DB = 'inner'; $h.head(1).Array }
is @seen.join(','), 'inner', 'head on a lazy gather';

@seen = ();
run({ my $*DB = 'eager'; (gather { take $_ for dyn-list() }).Array });
is @seen.join(','), 'eager', 'a non-lazy gather still works';
