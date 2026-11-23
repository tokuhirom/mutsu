use Test;
use experimental :rakuast;

plan 6;

# `RakuAST::Name.new` takes `:colonpairs`, so an operator's name
# (`infix:<+>`) can be built by hand, as in rakudo (#18441).

my $plus = RakuAST::Name.new(
    RakuAST::Name::Part::Simple.new("infix"),
    colonpairs => (RakuAST::QuotedString.new(segments => (RakuAST::StrLiteral.new("+"),)),),
);
like $plus.raku, /'RakuAST::Name::Part::Simple.new("infix"),' \s* 'colonpairs =>'/, '.raku';

my $call = RakuAST::Call::Name.new(
    name => $plus,
    args => RakuAST::ArgList.new(RakuAST::IntLiteral.new(1), RakuAST::IntLiteral.new(2)),
);
is $call.DEPARSE, 'infix:"+"(1, 2)', 'DEPARSE of a hand-built call';
is $call.EVAL, 3, 'EVAL calls the operator';

my $not = RakuAST::Name.new(
    RakuAST::Name::Part::Simple.new("prefix"),
    colonpairs => (RakuAST::QuotedString.new(
        segments   => (RakuAST::StrLiteral.new("!"),),
        processors => <words val>,
    ),),
);
is $not.DEPARSE, 'prefix:<!>', 'a word quote deparses as angle brackets';
is $not.raku, Q[prefix:<!>(1)].AST.statements[0].expression.name.raku,
    'the same node the parser builds';
is RakuAST::Call::Name.new(name => $not, args => RakuAST::ArgList.new(RakuAST::IntLiteral.new(1))).EVAL,
    False, 'EVAL of prefix:<!>(1)';

