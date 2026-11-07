use v6.e.PREVIEW;
use experimental :rakuast;
use Test;

# `RakuAST::Literal.from-value($v)` builds the literal node of the value's own
# type (Slang::Roman's number action ends with `make RakuAST::Literal.from-value($n)`).

plan 7;

isa-ok RakuAST::Literal.from-value(5), RakuAST::IntLiteral, 'an Int';
isa-ok RakuAST::Literal.from-value("s"), RakuAST::StrLiteral, 'a Str';
isa-ok RakuAST::Literal.from-value(1.5e0), RakuAST::NumLiteral, 'a Num';
isa-ok RakuAST::Literal.from-value(1.5), RakuAST::RatLiteral, 'a Rat';
is RakuAST::Literal.from-value(42).value, 42, 'the Int keeps its value';
is RakuAST::Literal.from-value("hi").value, 'hi', 'the Str keeps its value';
is EVAL(RakuAST::Literal.from-value(7)), 7, 'the node compiles back to the value';
