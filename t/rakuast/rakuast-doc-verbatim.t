use Test;

# The verbatim Pod block types (`code`, `input`, `output`, `comment`) as
# `RakuAST::Doc::Block` statements of `.AST` (#14880), shaped as rakudo 2026.09
# shapes them: a delimited block is ONE string of every line up to its `=end`
# (directives, indentation and inner blank lines kept; blank lines before the
# first line dropped); `code`, `input` and `output` still read formatting
# codes, `comment` never does.

plan 10;

use experimental :rakuast;

sub paras(Str $src) { $src.AST.statements[0].paragraphs.raku }
sub flat(Str $s) { $s.subst(/\s+/, ' ', :g) }

is paras("=begin code\nfirst\n\n  second\n\n\nthird\n\n=end code\n"),
    '("first\n\n  second\n\n\nthird\n\n",)', 'delimited code is one verbatim string';
is paras("=begin code\n\n\n  lead\n=end code\n"), '("  lead\n",)', 'leading blank lines are dropped';
is paras("=begin output\na\n=head1 not a directive\nb\n=end output\n"),
    '("a\n=head1 not a directive\nb\n",)', 'a directive inside is text';
is paras("=begin comment\n  indented\n    more\nplain\n=end comment\n"),
    '("  indented\n    more\nplain\n",)', 'a comment keeps its indentation';
is paras("=begin comment\nB<x> a\n=end comment\n"), '("B<x> a\n",)', 'a comment has no formatting codes';
is flat(paras("=begin code\nB<x> a\n=end code\n")),
    '(RakuAST::Doc::Paragraph.new( RakuAST::Doc::Markup.new( letter => "B", opener => "<", closer => ">", atoms => ( "x", ) ), " a\n" ),)',
    'code reads formatting codes';
is paras("=begin input\n=end input\n"), '()', 'an empty block has no paragraphs';
is paras("=comment abbreviated\n"), '("abbreviated\n",)', 'abbreviated comment';
is flat($_.AST.statements[0].raku), 'RakuAST::Doc::Block.new( type => "code", for => True, paragraphs => ( "line one\nline two\n\n", ) )',
    'a paragraph block' given "=for code\nline one\nline two\n\nsay 2;\n";
is EVAL("=begin code\nsay 1\n=end code\n2;\n".AST), 2, 'a tree with a verbatim block runs';
