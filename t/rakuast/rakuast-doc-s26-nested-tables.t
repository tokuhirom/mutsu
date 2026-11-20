use Test;
use experimental :rakuast;

# Pod shapes from the `roast/S26-documentation` differential, read as rakudo
# 2026.11 reads them (#14880): a line indented beyond a block's margin (kept
# by `head`/custom blocks, dropped by `item`), `=item # one` (numbered), a
# balanced `<...>` inside a formatting code, `E<...>` by Unicode name, an
# indented `=config`, and tables read with rakudo's own column algorithm
# (`!interpret-as-table`: virtual dividers, then visible ones).

plan 15;

sub docs(Str $src) { $src.AST.statements.grep(RakuAST::Doc::Block).map(*.raku).join("\n") }

is docs("=begin foo\n    =bold Turn\n          into\n          Jelly\n\nx\n=end foo\n"),
    "RakuAST::Doc::Block.new(\n  type       => \"foo\",\n  paragraphs => (\n    RakuAST::Doc::Block.new(\n      margin      => \"    \",\n      type        => \"bold\",\n      abbreviated => True,\n      paragraphs  => (\n        \"Turn\\n      into\\n      Jelly\\n\\n\",\n      )\n    ),\n    \"x\\n\",\n  )\n)",
    'an abbreviated block nested in a delimited one keeps its extra indentation';
is docs("    =head1 a\n      b\n\nsay 1;\n"),
    "RakuAST::Doc::Block.new(\n  margin      => \"    \",\n  type        => \"head\",\n  level       => 1,\n  abbreviated => True,\n  paragraphs  => (\n    \"a\\n  b\\n\\n\",\n  )\n)",
    'an indented `=head1` keeps a deeper line\'s blanks';
is docs("    =item a\n      b\n\nsay 1;\n"),
    "RakuAST::Doc::Block.new(\n  margin      => \"    \",\n  type        => \"item\",\n  abbreviated => True,\n  paragraphs  => (\n    \"a\\nb\\n\\n\",\n  )\n)",
    'an indented `=item` drops them';
is docs("=head1 a\n  b\n\nsay 1;\n"),
    "RakuAST::Doc::Block.new(\n  type        => \"head\",\n  level       => 1,\n  abbreviated => True,\n  paragraphs  => (\n    \"a\\n  b\\n\\n\",\n  )\n)",
    'so does a flat `=head1`, keeping';
is docs("=for item\na\n  b\n\nsay 1;\n"),
    "RakuAST::Doc::Block.new(\n  type       => \"item\",\n  for        => True,\n  paragraphs => (\n    \"a\\nb\\n\\n\",\n  )\n)",
    'and `=for item`, dropping';
is docs("=item # one\n\nsay 1;\n"),
    "RakuAST::Doc::Block.new(\n  type        => \"item\",\n  abbreviated => True,\n  config      => (\n    :numbered(RakuAST::IntLiteral.new(1)),\n  ),\n  paragraphs  => (\n    \"one\\n\\n\",\n  )\n)",
    '`=item # one` is numbered';
is docs("=defn # term\ndef\n\nsay 1;\n"),
    "RakuAST::Doc::Block.new(\n  type        => \"defn\",\n  abbreviated => True,\n  config      => (\n    :numbered(RakuAST::IntLiteral.new(1)),\n  ),\n  paragraphs  => (\n    \"term\",\n    \"def\\n\\n\",\n  )\n)",
    'so is `=defn # term`';
is docs("=pod C< infix:<+> > C<< < B<foo> > >>\n\nsay 1;\n"),
    "RakuAST::Doc::Block.new(\n  type        => \"pod\",\n  abbreviated => True,\n  paragraphs  => (\n    RakuAST::Doc::Paragraph.new(\n      RakuAST::Doc::Markup.new(\n        letter => \"C\",\n        opener => \"<\",\n        closer => \">\",\n        atoms  => (\n          \" infix:<+> \",\n        )\n      ),\n      \" \",\n      RakuAST::Doc::Markup.new(\n        letter => \"C\",\n        opener => \"<<\",\n        closer => \">>\",\n        atoms  => (\n          \" < \",\n          RakuAST::Doc::Markup.new(\n            letter => \"B\",\n            opener => \"<\",\n            closer => \">\",\n            atoms  => (\n              \"foo\",\n            )\n          ),\n          \" > \",\n        )\n      ),\n      \"\\n\\n\"\n    ),\n  )\n)",
    'a balanced `<...>` inside a code is its text';
is docs("=pod E<LATIN CAPITAL LETTER A> E<latin small letter b;nbsp>\n\nsay 1;\n"),
    "RakuAST::Doc::Block.new(\n  type        => \"pod\",\n  abbreviated => True,\n  paragraphs  => (\n    RakuAST::Doc::Paragraph.new(\n      RakuAST::Doc::Markup.new(\n        letter => \"E\",\n        opener => \"<\",\n        closer => \">\",\n        meta   => (\n          \"LATIN CAPITAL LETTER A\" => \"A\",\n        )\n      ),\n      \" \",\n      RakuAST::Doc::Markup.new(\n        letter => \"E\",\n        opener => \"<\",\n        closer => \">\",\n        meta   => (\n          \"latin small letter b\" => \"b\",\n          :nbsp(\" \"),\n        )\n      ),\n      \"\\n\\n\"\n    ),\n  )\n)",
    '`E<...>` by Unicode name';
is docs("=begin pod\n    =config head2  :like<head1>\n=end pod\n"),
    "RakuAST::Doc::Block.new(\n  type       => \"pod\",\n  paragraphs => (\n    RakuAST::Doc::Block.new(\n      margin     => \"    \",\n      type       => \"config\",\n      directive  => True,\n      config     => (\n        :like(RakuAST::QuotedString.new(\n          processors => <words val>,\n          segments   => (\n            RakuAST::StrLiteral.new(\"head1\"),\n          )\n        )),\n      ),\n      paragraphs => (\n        \"head2\",\n      )\n    ),\n  )\n)",
    'an indented `=config`';
is docs("=begin table\n-r0c0  r0c1\n=end table\n"),
    "RakuAST::Doc::Block.new(\n  type       => \"table\",\n  paragraphs => (\n    RakuAST::Doc::LegacyRow.new(\n      column-offsets => (0,7),\n      cells          => (\n        \"-r0c0\",\n        \"r0c1\",\n      )\n    ),\n  )\n)",
    'a hyphen-led cell';
is docs("=begin table\na-b  c\nd  e_f\n=end table\n"),
    "RakuAST::Doc::Block.new(\n  type       => \"table\",\n  paragraphs => (\n    RakuAST::Doc::LegacyRow.new(\n      column-offsets => (0,),\n      cells          => (\n        \"a-b  c\",\n      )\n    ),\n    RakuAST::Doc::LegacyRow.new(\n      column-offsets => (0,),\n      cells          => (\n        \"d  e_f\",\n      )\n    ),\n  )\n)",
    'no common gap: one column';
is docs("=begin table\na   b\nc  d\n=end table\n"),
    "RakuAST::Doc::Block.new(\n  type       => \"table\",\n  paragraphs => (\n    RakuAST::Doc::LegacyRow.new(\n      column-offsets => (0,3,4),\n      cells          => (\n        \"a\",\n        \" b\",\n        \"b\",\n      )\n    ),\n    RakuAST::Doc::LegacyRow.new(\n      column-offsets => (0,3,4),\n      cells          => (\n        \"c\",\n        \"d\",\n        \"\",\n      )\n    ),\n  )\n)",
    'a negative cell length takes the rest';
is docs("=begin table\nx = 1  y\n=end table\n"),
    "RakuAST::Doc::Block.new(\n  type       => \"table\",\n  paragraphs => (\n    RakuAST::Doc::LegacyRow.new(\n      column-offsets => (0,7),\n      cells          => (\n        \"x = 1\",\n        \"y\",\n      )\n    ),\n  )\n)",
    'an `=` inside a row';
is docs("=begin table\na | b-c\nd | e\n=end table\n"),
    "RakuAST::Doc::Block.new(\n  type       => \"table\",\n  paragraphs => (\n    RakuAST::Doc::LegacyRow.new(\n      column-dividers => \"|\",\n      column-offsets  => (4,),\n      cells           => (\n        \"a\",\n        \"b-c\",\n      )\n    ),\n    RakuAST::Doc::LegacyRow.new(\n      column-dividers => \"|\",\n      column-offsets  => (4,),\n      cells           => (\n        \"d\",\n        \"e\",\n      )\n    ),\n  )\n)",
    'a pipe table with a hyphen in a cell';
