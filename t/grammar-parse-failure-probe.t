use v6;
use Test;

plan 9;

# A failed `.parse` reports how far it got (`longest_complete_prefix_end`). That
# is a diagnostic, so computing it must neither execute the grammar's code atoms
# nor cost a re-match per prefix of the input. See docs/adr/0009.
#
# The tell is that the run count for a FIXED atom scales with the length of the
# input, even though that atom is only ever reached once. raku runs it once
# regardless.

our %n;

# TOP is fixed-length, so no prefix of a longer input ever matches it in full and
# the probe walks every prefix — day18's shape.
grammar G {
    token TOP  { <item>**3 }
    token item { (\w) <?{ %n{~$0}++; ~$0 ne 'z' }> }   # the assertion fails on 'z'
}

# All three inputs fail, and in all three the first character 'a' is reached
# exactly once by the real match. Only the input length differs.
%n = ();
nok G.parse("abz").defined, 'short input fails as expected';
my $short = %n<a>;

%n = ();
nok G.parse("abzdefghijklmnopqr").defined, 'long input fails as expected';
my $long = %n<a>;

ok $short >= 1, "the assertion ran for 'a'";
is $long, $short, "run count for 'a' does not scale with input length";

# Same property one subrule deeper, which is where advent2013-day18 sits: its
# card assertion ran 15 times on a 14-char input and 31 times on a 31-char one.
grammar Nested {
    token TOP   { <group>**2 % ';' }
    token group { <item>**2 }
    token item  { (\w) <?{ %n{~$0}++; ~$0 ne 'z' }> }
}

%n = ();
nok Nested.parse("ab;zc").defined, 'nested: short input fails as expected';
my $nshort = %n<a>;

%n = ();
nok Nested.parse("ab;zcdefghijklmnop").defined, 'nested: long input fails as expected';
is %n<a>, $nshort, 'nested: run count does not scale with input length either';

# The probe must also not re-match once per prefix. This is the shape that
# reaches it: the declarative skeleton matches (so LTM keeps the candidate and the
# real match runs) but the assertion rejects at the very end, and no prefix
# matches in full either. Each item's assertion is reached exactly once by the
# real match, so the total is the input length — in both implementations. A
# per-prefix probe made it grow with n on top of that.
our %total;
sub runs_for($len) {
    my grammar Late {
        token TOP  { ^ <item>+ $ }
        token item { (\w) <?{ %total<c>++; ~$0 ne 'z' }> }
    }
    %total = ();
    Late.parse(('a' x ($len - 1)) ~ 'z');
    return %total<c> // 0;
}
is runs_for(20), 20, 'assertion runs once per item, not once per item per prefix';
is runs_for(40), 40, '...and it stays exactly the input length as the input grows';

