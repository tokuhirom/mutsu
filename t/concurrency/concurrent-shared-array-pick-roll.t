use Test;

# `.pick` / `.roll` on an array other threads can reach read the live array
# under its lock rather than a copy of it (ADR-0068 §22), so they stay O(1)
# once a thread exists. They must still answer elements of the array while
# another thread grows it, and the Callable-count form (which runs user code)
# still works.

plan 7;

my @a = ^1000;
await start { 1 };

my @picked = (^200).map({ @a.pick });
ok @picked.all ~~ 0..^1000, '.pick answers elements of the array';
my @rolled = (^200).map({ @a.roll });
ok @rolled.all ~~ 0..^1000, '.roll answers elements of the array';
is @a.pick(5).unique.elems, 5, '.pick(5) answers distinct elements';
is @a.pick({ $_ div 100 }).elems, 10, 'a Callable count is called with .elems';

my @grow = ^10;
my $writer = start { @grow.push($_) for 10..^5010 };
my @seen;
@seen.push(@grow.pick) for ^5000;
await $writer;
ok @seen.all ~~ 0..^5010, '.pick during a concurrent push answers pushed elements';
is @grow.elems, 5010, 'and no push is lost';

my %h;
my @words = 'a' .. 'z';
await (^3).map({ start { for ^300 { my $w = @words.pick; %h{$w} = 1 } } });
ok %h.keys.all ~~ /^<[a..z]>$/, 'threads picking from one array';
