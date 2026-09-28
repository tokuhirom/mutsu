use Test;

# After `m:g`, `$/` is a List of Matches and `$N` is `$/[N]` -- the N-th
# match, not the first match's N-th capture. Found through Pod::To::HTML's
# t/070-headings.t, which does `$html ~~ m:g/ 'href="#Heading_3"' /; ok so $0`
# after an earlier `m:g/ ('2.2.2') /`.

plan 7;

my $s = 'a1b2c3';

$s ~~ m:g/ (\d) /;
is ~$0, '1', '$0 is the first match';
is ~$1, '2', '$1 is the second match';
is ~$0[0], '1', 'the match keeps its own capture';

$s ~~ m:g/ \d /;
is ~$0, '1', '$0 set by a capture-less m:g';
is ~$2, '3', '$2 set by a capture-less m:g';

$s ~~ m:g/ (\w) /;
$s ~~ m:g/ 'b2' /;
is ~$0, 'b2', 'an earlier match with captures leaves no stale $0';
nok $1.defined, 'and no stale $1';
