use Test;

# A formatting code right after a word character is still a code, as in
# rakudo's RakuDoc reader: `aB<b>` is the text `a` then `B<b>` (#19068).

plan 6;

sub atoms($text) {
    RakuAST::Doc::Paragraph.from-string($text).atoms.map({
        $_ ~~ Str ?? $_ !! "{.letter}<{.atoms.join}>"
    }).List
}

is-deeply atoms("aB<b> c"), ("a", "B<b>", " c"), 'after a letter';
is-deeply atoms("a1B<b>"), ("a1", "B<b>"), 'after a digit';
is-deeply atoms("aBC<d>"), ("aB", "C<d>"), 'only the letter before the opener starts the code';
is-deeply atoms("fooL<x|y>"), ("foo", "L<x>"), 'a code with metadata';

my $e = RakuAST::Doc::Paragraph.from-string("xE<gt> y");
is $e.atoms[1].letter, 'E', 'an entity right after a word character';

my $ast = "=begin pod\n\naB<b> c\n\n=end pod\n".AST;
is $ast.statements[0].paragraphs[0].atoms[1].letter, 'B',
    'a Pod block in source reads it too';
