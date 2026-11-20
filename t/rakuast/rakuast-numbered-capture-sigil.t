use Test;
use MONKEY-SEE-NO-EVAL;

# `@0` / `%0` are `Var::PositionalCapture` carrying the written sigil, not a
# `Contextualizer` over `$0`. Measured on Rakudo 2026.11 (#18252).

plan 11;

sub expr-of(Str $code) { ('"ab" ~~ /(a)(b)/; ' ~ $code).AST.statements[1].expression }

my $scalar = expr-of('$0');
is $scalar.^name, 'RakuAST::Var::PositionalCapture', '$0 is a positional capture';
is $scalar.sigil, '$', 'with the $ sigil';

my $list = expr-of('@0');
is $list.^name, 'RakuAST::Var::PositionalCapture', '@0 is a positional capture';
is $list.sigil, '@', 'with the @ sigil';
is $list.index, 0, 'of index 0';

my $hash = expr-of('%1');
is $hash.^name, 'RakuAST::Var::PositionalCapture', '%1 is a positional capture';
is $hash.sigil, '%', 'with the % sigil';

is expr-of('my @x = @0, @1').initializer.expression.operands.map(*.^name).unique.join(','),
    'RakuAST::Var::PositionalCapture', 'in a list';

for '"ab" ~~ /(a)(b)/; @1', '"ab" ~~ /(a)(b)/; @0.elems', '"ab" ~~ /(a)(b)/; %0.elems' -> $code {
    is-deeply EVAL($code.AST), EVAL($code), "EVAL of the tree of '$code' equals the source's";
}
