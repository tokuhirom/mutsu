use Test;

plan 57;

# Match's own methods are built-in method rows (ADR-11276 slice 3D). A regex
# match is lazy, so each scalar accessor answers from its capture node; a
# grammar cursor and a Match subclass have no row and still answer from the same
# implementation. Every expectation below was checked against Rakudo.
my $m = "hello world foo" ~~ /(\w+) \s+ $<second>=(\w+)/;

is $m.from, 0, 'from';
is $m.to, 11, 'to';
is $m.pos, 11, 'pos';
is $m.Str, 'hello world', 'Str';
is-deeply $m.Bool, True, 'Bool of a successful match';
is $m.orig, 'hello world foo', 'orig';
is $m.target, 'hello world foo', 'target';
is-deeply $m.made, Nil, 'made of a match with no made value';
is-deeply $m.ast, Nil, 'ast of a match with no made value';
is-deeply $m.clone, $m, 'clone is the match';
is $m.prematch, '', 'prematch of a match at the start';
is $m.postmatch, ' foo', 'postmatch';
nok $m.actions.defined, 'a plain match has no actions object';
is $m.caps.elems, 2, 'caps counts the captures';
is $m.caps[0].key, 0, 'the first capture is positional';
is $m.caps[1].key, 'second', 'the second capture is named';
is $m.chunks.elems, 3, 'chunks counts the stretches';
is $m.gist, '｢hello world｣' ~ "\n 0 => ｢hello｣\n second => ｢world｣", 'gist';
like $m.raku, /^ 'Match.new(' /, 'raku is a constructor call';

# The accessors do not need the match to be materialized, and a sub-match answers
# like a match.
is $m[0].Str, 'hello', 'a positional sub-match answers Str';
is $m[0].from, 0, 'a positional sub-match answers from';
is $m<second>.from, 6, 'a named sub-match answers from';
is $m<second>.to, 11, 'a named sub-match answers to';
is $m<second>.pos, 11, 'a named sub-match answers pos';
is $m<second>.prematch, 'hello ', 'a named sub-match answers prematch';
is $m<second>.postmatch, ' foo', 'a named sub-match answers postmatch';
is $m<second>.orig, 'hello world foo', 'a named sub-match answers orig';

# Every match of an adverbed match is a match.
my @all = "a1b2c3" ~~ m:g/\d/;
is @all.map(*.from).join(','), '1,3,5', 'from of each match of :g';
is @all.map(*.Str).join(','), '1,2,3', 'Str of each match of :g';
is @all.map(*.prematch).join(','), 'a,a1b,a1b2c', 'prematch of each match of :g';
is @all.map(*.postmatch).join(','), 'b2c3,c3,', 'postmatch of each match of :g';

# A zero-width match is true and empty; a failed match is Nil.
my $empty = "abc" ~~ /<?before a>/;
is-deeply $empty.Bool, True, 'a zero-width match is true';
is $empty.Str, '', 'a zero-width match is the empty string';
is $empty.from, 0, 'a zero-width match has from';
is $empty.to, 0, 'a zero-width match has to';
is-deeply $empty.so, True, 'so of a zero-width match';
is-deeply $empty.not, False, 'not of a zero-width match';
is-deeply ("abc" ~~ /x/), Nil, 'a failed match is Nil';

# Offsets count graphemes, not code points.
my $u = "日本語テキスト" ~~ /テキ/;
is $u.from, 3, 'from counts characters';
is $u.to, 5, 'to counts characters';
is $u.prematch, '日本語', 'prematch of a non-ASCII subject';
is $u.postmatch, 'スト', 'postmatch of a non-ASCII subject';
my $crlf = "a\r\nb" ~~ /b/;
is $crlf.from, 2, 'a CRLF counts as one character';

# A grammar parse is a cursor of the grammar's own class, and has no row.
grammar G { token TOP { <a> <b> }; token a { \d+ }; token b { \w+ } }
class A { method TOP($/) { make "top" }; method a($/) { make +$/ } }
my $g = G.parse("12ab", :actions(A.new));
is $g.^name, 'G', 'a grammar parse is a G';
is $g.from, 0, 'a cursor answers from';
is $g.to, 4, 'a cursor answers to';
is $g.Str, '12ab', 'a cursor answers Str';
is $g.made, 'top', 'a cursor answers made';
is $g.orig, '12ab', 'a cursor answers orig';
is $g<a>.made, 12, 'a sub-match of a cursor answers made';
is-deeply $g.Bool, True, 'a cursor answers Bool';
is $g<b>.prematch, '12', 'a sub-match of a cursor answers prematch';
ok $g.gist.starts-with('｢12ab｣'), 'a cursor answers gist';

# A user method wins over the row.
grammar H { token TOP { \d+ }; method made { 'user made' } }
is H.parse("12").made, 'user made', "a grammar's own made wins";

# The special variable is a match too.
"xyz" ~~ /y/;
is $/.from, 1, '$/.from';
is $/.postmatch, 'z', '$/.postmatch';
is ~$/, 'y', 'stringified $/';
