use v6;
use experimental :rakuast;
use Test;

# RakuAST slice 29 (ADR-0011): hash literals `{a => 1}` — read side (`.AST`).
# raku models `{...}` as a `Block` whose body is a `FatArrow` pair (or a comma
# list of `FatArrow`s), NOT a dedicated hash node. This file checks the `.AST`
# gist matches Rakudo. It is read-only: EVAL of the produced `Block` yields a
# block/Callable in raku (the block-vs-hash distinction is a parse-time decision
# the RakuAST node does not carry), so the write direction is out of scope.

plan 4;

# --- a single-entry hash literal --------------------------------------------
my $one = "\{a => 1}".AST.gist;
ok $one.contains('RakuAST::Block.new(')
    && $one.contains('RakuAST::FatArrow.new(')
    && $one.contains('key   => "a"')
    && $one.contains('RakuAST::IntLiteral.new(1)'),
    'a single-entry hash literal renders as a Block wrapping a FatArrow';

# --- a two-entry hash literal is a comma list of FatArrows ------------------
my $two = "\{a => 1, b => 2}".AST.gist;
ok $two.contains('RakuAST::ApplyListInfix.new(')
    && $two.contains('key   => "a"')
    && $two.contains('key   => "b"'),
    'a two-entry hash literal is a comma list of FatArrows';

# --- the block wraps a Blockoid / StatementList -----------------------------
ok $two.contains('RakuAST::Blockoid.new(')
    && $two.index('RakuAST::Block.new(') < $two.index('RakuAST::FatArrow.new('),
    'the pairs live inside the block body';

# --- a string-value entry ---------------------------------------------------
my $str = "\{name => \"x\"}".AST.gist;
ok $str.contains('RakuAST::FatArrow.new(')
    && $str.contains('key   => "name"')
    && $str.contains('RakuAST::StrLiteral.new("x")'),
    'a hash entry with a string value';
