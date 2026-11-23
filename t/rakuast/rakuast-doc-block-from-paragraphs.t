use Test;

# RakuAST::Doc::Block.from-paragraphs and a single-item Doc::Markup :atoms
# (#18981); every expectation was taken from rakudo 2026.11.

plan 16;

my $b = RakuAST::Doc::Block.from-paragraphs(:margin("  "), :type<foo>, :paragraphs("  bar\n",));
is-deeply $b.paragraphs, ("bar\n",), 'the margin is stripped';
is $b.DEPARSE, "  =begin foo\n\n  bar\n  =end foo\n\n", 'deparses with the margin';

throws-like { RakuAST::Doc::Block.from-paragraphs(:margin("  "), :type<foo>, :paragraphs(" z\n",)) },
  Exception, message => "' z'\n    does not have enough whitespace to allow for a margin of 2 positions",
  'too little whitespace dies';
is-deeply RakuAST::Doc::Block.from-paragraphs(:margin(" "), :type<foo>, :paragraphs(" a\n\n b\n",)).paragraphs,
  ("a\nb\n",), 'a blank line is a bare newline';

my $code = RakuAST::Doc::Block.from-paragraphs(:type<code>, :paragraphs("my B<\$x>\n",));
isa-ok $code.paragraphs[0], RakuAST::Doc::Paragraph, 'code reads formatting codes';
is-deeply RakuAST::Doc::Block.from-paragraphs(:type<comment>, :paragraphs("B<x>\n",)).paragraphs,
  ("B<x>\n",), 'a comment is verbatim';

my $item = RakuAST::Doc::Block.from-paragraphs(:type<item>,
  :paragraphs("text here\n\n  code1\n  code2\n\nmore I<x>\n",));
my @p = $item.paragraphs;
is @p.elems, 3, 'item: text, implicit code, text';
is @p[0], "text here\n\n", 'item: the text';
is @p[1].type, 'implicit-code', 'item: an implicit code block';
is @p[1].margin, '  ', 'item: the code block keeps its indent as margin';
isa-ok @p[2], RakuAST::Doc::Paragraph, 'item: markup after the code';

my $defn = RakuAST::Doc::Block.from-paragraphs(:type<defn>, :paragraphs("lemma\nthe def\n\n    code\n",));
is $defn.paragraphs[0], 'lemma', 'defn: the lemma';
is $defn.paragraphs[2].type, 'implicit-code', 'defn: code after the definition';

my $table = RakuAST::Doc::Block.from-paragraphs(:type<table>, :paragraphs("a | b\n=====\n1 | 2\n",));
isa-ok $table.paragraphs[0], RakuAST::Doc::LegacyRow, 'table: rows';

my $m = RakuAST::Doc::Markup.new(:letter<B>, :atoms("and"));
is $m.DEPARSE, 'B<and>', 'Markup.new takes a single atom';
is-deeply RakuAST::Doc::Markup.new(:letter<B>, :meta("x")).meta, ("x",), 'and a single meta';
