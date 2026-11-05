use Test;

# Formatting codes with metadata in a Pod paragraph (#14880): `X<a|b;c>`,
# `L<text|url>`, `D<..|..>` (and `A F M P`) keep the text after the first `|`
# as a one-string `meta` list; the letters without metadata keep the `|` in
# their atoms. Shapes measured on rakudo 2026.09.

plan 9;

use experimental :rakuast;

sub markups(Str $src) {
    $src.AST.statements[0].paragraphs[0].atoms.grep(RakuAST::Doc::Markup)
}
sub shape($m) { "{$m.letter}:{$m.atoms.raku}:{$m.meta.raku}" }

my @m = markups("=begin pod\nHello X<a|b;c> and L<text|http://x.org> D<foo|bar>\n=end pod\n");
is @m.map(&shape).join(' '), 'X:("a",):("b;c",) L:("text",):("http://x.org",) D:("foo",):("bar",)',
    'X, L and D take their metadata';
is shape(markups("=begin pod\nX<a|b|c>\n=end pod\n")[0]), 'X:("a",):("b|c",)', 'split at the first |';
is shape(markups("=begin pod\nL<|x>\n=end pod\n")[0]), 'L:():("x",)', 'no text, only metadata';
is shape(markups("=begin pod\nL<a|>\n=end pod\n")[0]), 'L:("a",):("",)', 'empty metadata';
is shape(markups("=begin pod\nX<B<z>|y>\n=end pod\n")[0]).subst(/\s+/, ' ', :g).substr(0, 1), 'X', 'a nested code in the text';
is shape(markups("=begin pod\nB<a|b>\n=end pod\n")[0]), 'B:("a|b",):()', 'B keeps the | in its text';
is shape(markups("=begin pod\nP<a|b>\n=end pod\n")[0]), 'P:("a",):("b",)', 'P takes metadata';
is markups("=begin pod\nL<<a|b>>\n=end pod\n")[0].opener, '<<', 'with the doubled delimiters';
is markups("=begin pod\nL<a|b>\n=end pod\n")[0].raku.subst(/\s+/, ' ', :g),
    'RakuAST::Doc::Markup.new( letter => "L", opener => "<", closer => ">", atoms => ( "a", ), meta => ( "b", ) )',
    'and prints them';
