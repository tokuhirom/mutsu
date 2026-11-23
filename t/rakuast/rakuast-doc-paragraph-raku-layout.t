use Test;

# A `Doc::Paragraph` with any atoms puts them on their own lines in `.raku`,
# even a single string; an empty one stays `.new()` (#19039).

plan 4;

is RakuAST::Doc::Paragraph.new("q").raku,
    qq{RakuAST::Doc::Paragraph.new(\n  "q"\n)},
    'a one-atom paragraph breaks over lines';

is RakuAST::Doc::Paragraph.new.raku, 'RakuAST::Doc::Paragraph.new()',
    'an empty paragraph stays on one line';

is RakuAST::Doc::Paragraph.new("a", "b").raku,
    qq{RakuAST::Doc::Paragraph.new(\n  "a",\n  "b"\n)},
    'several atoms break over lines';

is RakuAST::Doc::Block.new(:type<pod>, :paragraphs(RakuAST::Doc::Paragraph.new("q"),)).raku,
    qq{RakuAST::Doc::Block.new(\n  type       => "pod",\n  paragraphs => (\n    RakuAST::Doc::Paragraph.new(\n      "q"\n    ),\n  )\n)},
    'a nested one-atom paragraph breaks over lines';
