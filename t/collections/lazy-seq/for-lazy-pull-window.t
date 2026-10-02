use Test;

# A `for` loop over a lazy source, and a pipe stage over a lazy source, pull
# only the elements they are about to consume instead of copying the whole
# reified prefix on every pull (#10780). These pin the observable behaviour
# that the windowed pull must keep: interleaving, chunking, the cache that
# later reads see, and error / override handling.

plan 14;

{
    my @log;
    for gather { for 1..* { @log.push("t$_"); take $_ } } { @log.push("b$_"); last if $_ >= 3 }
    is @log.join(' '), 't1 b1 t2 b2 t3 b3', 'gather takes and the loop body interleave';
}
{
    my $g = gather { take $_ for 1..5 };
    my @seen;
    for $g.list -> $a, $b? { @seen.push("$a/{$b // '-'}") }
    is @seen.join(' '), '1/2 3/4 5/-', 'two-parameter loop over a gather, partial last chunk';
}
{
    my $g = (gather { take $_ * 10 for 1..6 }).cache;
    my @seen;
    for $g.list { @seen.push($_); last if $_ >= 30 }
    is @seen.join(' '), '10 20 30', 'loop stops early';
    is $g[^6].join(' '), '10 20 30 40 50 60', 'the resumed gather keeps its already-pulled prefix';
}
{
    my @seen;
    try { for gather { take 1; take 2; die "boom"; take 3 } { @seen.push($_) } }
    is @seen.join(' '), '1 2', 'elements before a die in the gather body are seen';
    is $!.message, 'boom', 'the die propagates out of the loop';
}
{
    my @seen;
    for (1, 1, * + * ... *) { @seen.push($_); last if @seen == 8 }
    is @seen.join(' '), '1 1 2 3 5 8 13 21', 'closure sequence pulled one element at a time';
}
{
    my @fib = 1, 1, * + * ... *;
    is @fib[^6].join(' '), '1 1 2 3 5 8', 'closure sequence prefix';
    @fib[2] = 99;
    is @fib[^8].join(' '), '1 1 99 3 5 8 13 21', 'an element override does not feed later terms';
}
{
    my @seen;
    for ([\+] (1..*).map(* * 2)) { @seen.push($_); last if @seen == 5 }
    is @seen.join(' '), '2 6 12 20 30', 'triangle reduce over a lazy pipe';
}
{
    my @log;
    my $g = gather { for 1..* { @log.push("t$_"); take $_ } };
    for $g.map({ @log.push("m$_"); $_ * 2 }) { @log.push("b$_"); last if $_ >= 4 }
    is @log.join(' '), 't1 m1 b2 t2 m2 b4', 'a map stage pulls its gather source one element at a time';
}
{
    my @seen;
    for (1..*).grep(* %% 3).map(* + 1) { @seen.push($_); last if @seen == 4 }
    is @seen.join(' '), '4 7 10 13', 'chained grep/map pipe';
}
{
    my $n = 0;
    for (1..*).map(*+0) { $n++; last if $_ >= 50_000 }
    is $n, 50_000, 'a long lazy pipe loop completes';
}
{
    my $n = 0;
    for gather { my $i = 0; loop { take $i++ } } { $n++; last if $_ >= 50_000 }
    is $n, 50_001, 'a long lazy gather loop completes';
}
