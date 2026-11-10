use Test;
use experimental :rakuast;

# `.<k>` / `.[i]` / `.{k}` on the topic is `Term::TopicCall` over the bare
# subscript, where `$_<k>` is an `ApplyPostfix` on `$_`. Identical under rakudo.
plan 11;

sub expr(Str $code) { $code.AST.statements[0].expression }

for ｢.<amount>｣, ｢.[0]｣, ｢.{'a'}｣, ｢.<a b>｣ -> $code {
    isa-ok expr($code), RakuAST::Term::TopicCall, "$code is a TopicCall";
}
isa-ok expr(｢$_<amount>｣), RakuAST::ApplyPostfix, '$_<amount> stays an ApplyPostfix';
isa-ok expr(｢.<a>｣).call, RakuAST::Postcircumfix::LiteralHashIndex, '.<a> holds a LiteralHashIndex';
isa-ok expr(｢.[0]｣).call, RakuAST::Postcircumfix::ArrayIndex, '.[0] holds an ArrayIndex';
isa-ok expr(｢.{'a'}｣).call, RakuAST::Postcircumfix::HashIndex, '.{"a"} holds a HashIndex';
isa-ok expr(｢.<a>.foo｣).operand, RakuAST::Term::TopicCall, 'a method on a topic subscript';

# The nodes still run.
my %h = a => 5;
is (do given %h { .<a> }), 5, '.<a> reads the topic hash';
my @a = 7, 8;
is (do given @a { .[1] }), 8, '.[1] reads the topic array';
