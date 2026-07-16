use v6;
use Test;

# A subrule is matched against a `chars[pos..]` slice, so its captures come back
# slice-relative. Rakudo's `.from`/`.to` are ALWAYS absolute to the original
# string — even for a subrule matched deep inside a repetition. Regression test
# for nested/positional offsets not being rebased to absolute (Template::Mustache
# section lambdas depend on this: `$/.substr($f<pos>, $hunk<pos> - $f<pos>)`).

plan 12;

grammar G {
    regex TOP  { ^ <hunk>* (.*) $ }
    regex hunk { (.*?) <tag> }
    proto regex tag {*}
    regex tag:sym<x> { 'XX' }
    regex tag:sym<y> { 'YY' }
}

my $m = G.parse('aXXbYY');
my @h = $m<hunk>.list;

# First hunk (matched at pos 0): offsets already absolute.
is @h[0].from, 0, 'hunk[0].from';
is @h[0].to,   3, 'hunk[0].to';
is @h[0][0].from, 0, 'hunk[0] $0.from';
is @h[0]<tag>.from, 1, 'hunk[0] tag.from';
is @h[0]<tag>.to,   3, 'hunk[0] tag.to';

# Second hunk (matched at slice pos 3): the nested tag and the subrule's own
# $0 must be shifted to absolute, not left slice-relative.
is @h[1].from, 3, 'hunk[1].from';
is @h[1].to,   6, 'hunk[1].to';
is @h[1][0].from, 3, 'hunk[1] $0.from (was slice-relative 0)';
is @h[1][0].to,   4, 'hunk[1] $0.to (was slice-relative 1)';
is @h[1]<tag>.from, 4, 'hunk[1] tag.from (was slice-relative 1)';
is @h[1]<tag>.to,   6, 'hunk[1] tag.to (was slice-relative 3)';

# The absolute offsets must make substr of the top string work.
is $m.orig.substr(@h[1]<tag>.from, @h[1]<tag>.to - @h[1]<tag>.from), 'YY',
    'substr with absolute nested offsets';
