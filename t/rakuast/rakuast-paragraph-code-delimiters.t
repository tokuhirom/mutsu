use Test;

# The RakuDoc reader takes every formatting-code shape rakudo's does (#19091):
# an empty code, any run of `<` closed by the same run of `>`, metadata with
# balanced brackets, and an `E<...>` whose text names no entity.

plan 12;

sub markup($text) {
    RakuAST::Doc::Paragraph.from-string($text).atoms.first(* !~~ Str)
}

my $m = markup("a B<> c");
is $m.letter, 'B', 'an empty code is a Markup';
is $m.atoms.elems, 0, '... with no atoms';

$m = markup("a B<<<>>> c");
is $m.opener, '<<<', 'an empty triple-angle code';

$m = markup("a B<<<a|b>>> c");
is-deeply $m.atoms.List, ("a|b",), 'a bar in a code without metadata is text';

$m = markup("a L<<<x|y>>> c");
is-deeply ($m.atoms.List, $m.meta.List), (("x",), ("y",)), 'metadata in a triple-angle code';

$m = markup("a B<<<<x>>>> c");
is ($m.opener, $m.closer).join(' '), '<<<< >>>>', 'four angles close with four';

$m = markup("a B<<<x C<y> z>>> c");
is $m.atoms[1].letter, 'C', 'a code nested in a triple-angle code is read';

$m = markup("a B<<< x > y >>> c");
is-deeply $m.atoms.List, (" x > y ",), 'a lone > inside a longer closer is text';

$m = markup("a L<x|y<z>> c");
is-deeply $m.meta.List, ("y<z>",), 'metadata keeps balanced brackets';

$m = markup("a E<lt<x>> c");
is-deeply $m.atoms.List, ("lt<x>",), 'an E code naming no entity keeps its text';

throws-like { RakuAST::Doc::Paragraph.from-string("a B<<<<x>>> c") }, Exception,
    message => "RakuDoc markup code B missing endtag '>>>>'.",
    'a run of four angles needs four to close';

$m = markup("a B«x» c");
is ($m.opener, $m.closer).join(' '), '« »', 'a guillemet code';
