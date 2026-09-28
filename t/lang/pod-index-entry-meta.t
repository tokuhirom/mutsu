use Test;

# `X<text|entries>`: `;` separates entries and `,` separates an entry's
# levels. rakudo skips only the whitespace around the `|`; each level keeps
# its own spaces. Found through Pod::To::HTML's t/050-format-x-index.t
# (`index-entry-defining__a_term-hierarchical_items`).

plan 5;

=begin pod
X<a|defining, a term> X<b|Same; Place> X<c|  single> X<d|x,y;> X<e|p ,q>
=end pod

my @meta = $=pod[0].contents[0].contents.grep(Pod::FormattingCode)».meta;

is-deeply @meta[0], [["defining", " a term"],], 'a level keeps its leading space';
is-deeply @meta[1], [["Same"], [" Place"]], 'an entry keeps its leading space';
is-deeply @meta[2], [["single"],], 'whitespace after the | is skipped';
is-deeply @meta[3], [["x", "y"],], 'a trailing ; adds no empty entry';
is-deeply @meta[4], [["p ", "q"],], 'trailing space before a , is kept';
