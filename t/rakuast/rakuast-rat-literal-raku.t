use Test;

# A RatLiteral renders its value's `.raku`: every digit, and `.0` for a whole
# value (rakudo 2026.09). It rendered the value's `.Str`, rounded to the
# `.Str` precision (`RatLiteral.new(1)`, `-0.00007499522`).

plan 4;

is RakuAST::RatLiteral.new(1.0).raku, 'RakuAST::RatLiteral.new(1.0)', 'a whole value keeps .0';
is Q[3.0].AST.statements[0].expression.raku, 'RakuAST::RatLiteral.new(3.0)', 'a parsed whole value';
is Q[-0.0000749952159821987152099609375].AST.statements[0].expression.operand.raku,
    'RakuAST::RatLiteral.new(0.0000749952159821987152099609375)', 'every digit';
is EVAL(Q[1.5].AST.statements[0].expression.raku).value, 1.5, 'the rendering round-trips';
