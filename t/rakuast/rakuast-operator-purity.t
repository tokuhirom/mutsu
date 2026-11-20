use Test;
use experimental :rakuast;

# `is-pure` and `short-circuit` of the operator nodes, measured on Rakudo 2026.11
# (#14916).

plan 28;

sub infix($s) { RakuAST::Infix.new($s) }

is-deeply infix($_).is-pure, True, "infix $_ is pure" for <+ ~ == && = ::= min Z>;
is-deeply infix($_).is-pure, False, "infix $_ is not pure" for <does xx ~~ :=>;

is-deeply infix($_).short-circuit, True, "infix $_ short-circuits" for <&& || // and or andthen>;
is-deeply infix($_).short-circuit, False, "infix $_ does not short-circuit" for <+ ^^ ff>;

is-deeply RakuAST::Prefix.new("-").is-pure, True, 'prefix - is pure';
is-deeply RakuAST::Prefix.new("++").is-pure, False, 'prefix ++ is not pure';
is-deeply RakuAST::Assignment.new.is-pure, False, 'an Assignment is not pure';
is-deeply RakuAST::MetaInfix::Assign.new(infix("+")).is-pure, False, 'a meta-assign is not pure';
is-deeply RakuAST::MetaInfix::Negate.new(infix("==")).is-pure, True, 'a negated infix is pure';
is-deeply RakuAST::MetaInfix::Zip.new(infix("+")).short-circuit, False, 'a zip does not short-circuit';
is-deeply RakuAST::Postfix.new(operator => "++").can("is-pure").elems, 0, 'a postfix has no is-pure';
