use v6;
use Test;

# A Pod formatting code may be delimited by `«…»` or by any run of angles
# (`<<…>>`, `<<<…>>>`), not only by `<…>` (S26). From rakudoc 0.2.6, whose test
# docs index `=head2 postcircumfix C«( )»`; mutsu read the heading as plain
# text "postcircumfix C«( )»". Expected structure checked against raku. Only
# leading whitespace differs there: roast S26-documentation/08-formattingcodes.t
# requires C<> contents to keep it, which rakudo fails, so this test avoids it.

plan 12;

=begin pod
=head2 postcircumfix C«( )»

C«x» B«a<b>c» I«a B<x> c» C<<<a >> b>>> C<<x>>> C«a»» L«t|u»
=end pod

my $heading = $=pod[0].contents[0];
my @h = $heading.contents[0].contents;
is @h.elems, 2, 'a «…» code in a heading is parsed';
is @h[0], 'postcircumfix ', '... after the plain text';
is-deeply (@h[1].type, @h[1].contents.List), ('C', ('( )',)), '... as C«( )»';

my @p = $=pod[0].contents[1].contents.grep(Pod::FormattingCode);
is-deeply @p.map(*.type).List, <C B I C C C L>, 'every code in the paragraph is found';
is @p[0].contents[0], 'x', 'C«x»';
is @p[1].contents[0], 'a<b>c', 'angles inside «…» are text';
is @p[2].contents[1].type, 'B', 'a code nested inside «…» is still parsed';
is @p[3].contents[0], 'a >> b', 'a triple-angle code closes at the first >>>';
is @p[4].contents[0], 'x', 'C<<x>>> closes at the first >>';
is @p[5].contents[0], 'a', 'C«a»» closes at the first »';
is-deeply (@p[6].contents.List, @p[6].meta.List), (('t',), ('u',)), 'L«t|u» splits at the |';

my @rest = $=pod[0].contents[1].contents.grep(Str).map(*.trim).grep(*.chars);
is-deeply @rest.List, ('>', '»'), 'the closing delimiter left over is text';
