use Test;
plan 8;
my $l = Lock.new;
my $x = 0;
for ^5 { $l.protect: { $x++ } }
is $x, 5, 'a protect block writes back to a captured outer scalar in a loop';

sub counter($lock) {
    my $n = 0;
    for ^3 { $lock.protect: { $n += 2 } }
    $n
}
is counter($l), 6, 'each call of a sub gets its own captured local';
is counter($l), 6, 'and again (memoized bindings, fresh env)';

my @log;
for 1..3 -> $i {
    $l.protect: { @log.push($i * 10) };
}
is @log, [10, 20, 30], 'a loop variable captured per iteration is read correctly';

my $total = 0;
for 1..4 -> $i {
    my $sq = $i * $i;
    $l.protect: { $total += $sq };
}
is $total, 30, 'a per-iteration lexical that exists in some closures only';

{
    my $inner = 100;
    $l.protect: { $inner++ };
    is $inner, 101, 'a block-scoped lexical';
}
our $g = 1;
$l.protect: { $g += 4 };
is $g, 5, 'a package variable';
my $r = $l.protect: { 42 };
is $r, 42, 'the block value is returned';
