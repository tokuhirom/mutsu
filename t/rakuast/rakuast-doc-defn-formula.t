use Test;

# `=defn` and delimited / `=for` `=formula` blocks (#14880). A `defn` starts
# with its term, the first line, as a plain string; the rest are the
# paragraphs of its definition. Shapes measured on rakudo 2026.09.

plan 8;

use experimental :rakuast;

sub block(Str $src) { $src.AST.statements[0].raku.subst(/\s+/, ' ', :g) }

is block("=defn Term\nthe definition\nmore\n"),
    'RakuAST::Doc::Block.new( type => "defn", abbreviated => True, paragraphs => ( "Term", "the definition\nmore\n", ) )',
    'an abbreviated defn';
is block("=for defn\nTerm\nthe def\n"),
    'RakuAST::Doc::Block.new( type => "defn", for => True, paragraphs => ( "Term", "the def\n", ) )',
    'a for defn';
is block("=begin defn\nTerm\nthe def\n\nsecond para\n=end defn\n"),
    'RakuAST::Doc::Block.new( type => "defn", paragraphs => ( "Term", "the def\n\n", "second para\n", ) )',
    'a delimited defn with two paragraphs';
is block("=begin defn\nTerm\n=end defn\n"),
    'RakuAST::Doc::Block.new( type => "defn", paragraphs => ( "Term", ) )',
    'a defn with only a term';
is block("=for defn\nTerm B<x>\nmore\n"),
    'RakuAST::Doc::Block.new( type => "defn", for => True, paragraphs => ( "Term B<x>", "more\n", ) )',
    'the term is not read for formatting codes';
is block("=defn T\nthe E<lt> def\n"),
    'RakuAST::Doc::Block.new( type => "defn", abbreviated => True, paragraphs => ( "T", RakuAST::Doc::Paragraph.new( "the ", RakuAST::Doc::Markup.new( letter => "E", opener => "<", closer => ">", meta => ( :lt("<"), ) ), " def\n" ), ) )',
    'the definition is';
is block("=begin formula\nx = 1\n=end formula\n"),
    'RakuAST::Doc::Block.new( type => "formula", paragraphs => ( "x = 1\n", ) )',
    'a delimited formula';
is block("=for formula\nx = 1\n"),
    'RakuAST::Doc::Block.new( type => "formula", for => True, paragraphs => ( "x = 1\n", ) )',
    'a for formula';
