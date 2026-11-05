use Test;

# `.Str` of the `RakuAST::Doc` nodes is the text of the Pod (#14880), as
# rakudo 2026.09 answers it: a block is its paragraphs run together, a
# formatting code is rewritten with its delimiters and a table row is put back
# as written.

plan 20;

use experimental :rakuast;

sub text(Str $src) { $src.AST.statements[0].Str }

is text("=begin pod\nhello\n=end pod\n"), "hello\n", 'a plain paragraph';
is text("=begin pod\nfirst\n\nsecond\n=end pod\n"), "first\n\nsecond\n", 'two paragraphs';
is text("=begin pod\nB<x> y\n=end pod\n"), "B<x> y\n", 'a formatting code';
is text("=begin pod\nC<<a b>> B«x»\n=end pod\n"), "C<<a b>> B«x»\n", 'other delimiters';
is text("=begin pod\nX<a|b;c;d> D<a|b> F<a|b>\n=end pod\n"), "X<a|b;c;d> D<a|b> F<a|b>\n", 'codes with metadata';
is text("=begin pod\nE<gt;lt> E<0x41> E<foo>\n=end pod\n"), "E<gt;lt> E<0x41> E<foo>\n", 'entity codes';
is text("=begin pod\n=head1 T\n\ntext\n=end pod\n"), "T\n\ntext\n", 'a nested block';
is text("=begin pod\n  code here\n=end pod\n"), "code here\n", 'implicit code drops its margin';
is text("=begin code\n  x\n  y\n=end code\n"), "  x\n  y\n", 'a code block keeps its indentation';
is text("=begin defn\nTerm\nthe def\n=end defn\n"), "Termthe def\n", 'a defn runs its term into the definition';
is text("=config head1 :numbered\n"), "head1", 'the config directive';
is text("=begin table\nA  B\nC  D\n=end table\n"), "A  B\nC  D\n", 'a whitespace table';
is text("=begin table\nA   B   C\nD   E   F\n=end table\n"), "A   B   C\nD   E   F\n", 'wide gaps stay';
is text("=begin table\n| A | B |\n| C | D |\n=end table\n"), "| A | B |\n| C | D |\n", 'a pipe table with outer dividers';
is text("=begin table\nA |  B\nCC  | D\n=end table\n"), "A |  B\nCC  | D\n", 'a pipe table with uneven cells';
is text("=begin table\nA  B\nC  D\n--\nE  F\nG  H\n=end table\n"), "A  B\nC  D\n--E  F\nG  H\n", 'a multi-line header row and a rule';

is RakuAST::Doc::Markup.new(letter => "X", atoms => ("a",), meta => ("b", "c")).Str, "X<a|b c>", 'hand-built metadata';
is RakuAST::Doc::Markup.new(letter => "E", opener => "«", closer => "»", meta => (:gt(">"),)).Str, "E«gt»", 'hand-built entity code';
is RakuAST::Doc::Block.new(type => "foo", paragraphs => ("p1\n", "p2")).Str, "p1\np2", 'hand-built block';
is RakuAST::Doc::LegacyRow.new(column-offsets => (0, 8), cells => ("A", "B")).Str, "A  B\n", 'hand-built row';
