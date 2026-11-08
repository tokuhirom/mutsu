use v6.e.PREVIEW;
use Test;

# `RakuAST::Blockoid.new` without arguments builds a Blockoid over an empty
# StatementList, as on rakudo 2026.09 (#17147).

plan 3;

my $empty = RakuAST::Blockoid.new;
is $empty.^name, 'RakuAST::Blockoid', 'built without arguments';
is $empty.raku, RakuAST::Blockoid.new(RakuAST::StatementList.new).raku,
    'the same as one over an explicit empty StatementList';
is $empty.raku.lines.elems, 3, 'printed over three lines';
