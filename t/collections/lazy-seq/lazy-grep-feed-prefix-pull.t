use Test;

# A deferred `.grep` pulled a prefix at a time (a `for` loop, a chained
# `.map`, `.head`, an Iterator) runs until it has the matches the pull needs,
# reading the source as it reaches each element, instead of one source
# element per loop run (#11515). What it may not change: which elements the
# callback sees, the order the callbacks interleave in, and the write-back.

plan 15;

{
    my @log;
    for (1..7).grep({ @log.push("g$_"); $_ %% 3 }) { @log.push("b$_") }
    is @log.join(' '), 'g1 g2 g3 b3 g4 g5 g6 b6 g7',
        'a sparse grep interleaves with the loop body';
}
{
    my @log;
    my @a = 1..10;
    my $s = @a.grep({ @log.push("g$_"); $_ %% 4 }).map({ @log.push("m$_"); $_ + 1 });
    is $s.head(2).join(' '), '5 9', 'a chained sparse grep.map produces its prefix';
    is @log.join(' '), 'g1 g2 g3 g4 m4 g5 g6 g7 g8 m8',
        '... running the grep only as far as the prefix needs';
}
{
    my $c = 0;
    my @a = 1..100;
    is @a.grep({ $c++; $_ > 50 }).head(1).join, '51', 'head(1) of a late match';
    is $c, 51, '... ran the callback over exactly the elements it needed';
}
{
    my @got;
    for (1..20).grep(/3|7/) { @got.push($_) }
    is @got.join(' '), '3 7 13 17', 'a Regex matcher streams too';
}
{
    my @a = 1..5;
    my @seen;
    for @a.grep({ $_ > 1 }) { @a.push(99) if @a.elems < 8; @seen.push($_) }
    is @seen.join(' '), '2 3 4 5 99 99 99',
        'elements pushed onto the source while it streams are seen';
}
{
    my @a = 1..6;
    for @a.grep(* %% 2) { $_ *= 10 }
    is @a.join(' '), '1 20 3 40 5 60', 'the loop writes back through the matched slots';
}
{
    my @a = 1..10;
    is @a.grep({ last if $_ > 7; $_ %% 3 }).map(* * 2).join(' '), '6 12',
        '`last` in the upstream grep of a chain ends it';
}
{
    my @log;
    for (1..6).grep({ @log.push("g$_"); $_ %% 2 }) {
        @log.push("b$_");
        last if $_ == 4;
    }
    is @log.join(' '), 'g1 g2 b2 g3 g4 b4', '`last` in the loop leaves the rest ungrepped';
}
{
    sub over($min, $x) { $x > $min }
    my @got;
    for (1..8).grep(&over.assuming(5)) { @got.push($_) }
    is @got.join(' '), '6 7 8', 'an .assuming callback streams through the chunked path';
}
{
    my $it = (1..12).grep(* %% 5).iterator;
    is $it.pull-one, 5, 'Iterator pull-one finds the first match';
    is $it.pull-one, 10, '... and the next one';
}
{
    my @a = 1..9;
    my $s = @a.grep({ $_ %% 4 });
    ok ?$s, 'boolifying a sparse grep pulls one match';
    is $s.join(' '), '4 8', '... and the rest still arrives afterwards';
}
