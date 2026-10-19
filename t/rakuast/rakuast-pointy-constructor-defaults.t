use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;
plan 20;

sub body($value) {
    RakuAST::Blockoid.new(RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new($value))))
}
my $empty = RakuAST::PointyBlock.new;
is $empty.signature.^name, 'RakuAST::Signature', 'default signature class';
ok $empty.signature.defined, 'default signature is a defined empty node';
is $empty.signature.parameters.elems, 0, 'default signature has no parameters';
is $empty.body.^name, 'RakuAST::Blockoid', 'default body class';
is $empty.body.statement-list.statements.elems, 0, 'default body is empty';
nok $empty.gist.contains('signature'), 'absent signature is omitted';
my &empty_callable = EVAL(RakuAST::PointyBlock.new);
is &empty_callable.^name, 'Block', 'a default pointy node evaluates to a callable';
is &empty_callable.arity, 0, 'the default signature takes no arguments';
is empty_callable().raku, 'Nil', 'empty callable returns Nil';
dies-ok { empty_callable(1) }, 'empty callable rejects arguments';
my $explicit = RakuAST::PointyBlock.new(signature => RakuAST::Signature.new);
nok $explicit.gist.contains('signature'), 'explicit empty signature is also omitted';
is $explicit.body.statement-list.statements.elems, 0, 'explicit signature still gets an empty body';
my &body_callable = EVAL(RakuAST::PointyBlock.new(body => body(7)));
is body_callable(), 7, 'body-only constructor stays callable';
dies-ok { body_callable(1) }, 'body-only constructor rejects excess arguments';
is EVAL(Q[my &f = -> { 7 }; f()].AST), 7, 'source empty pointy signature round trips';
is EVAL(Q[my &f = -> Int $x --> Int { $x + 1 }; f(4)].AST), 5,
    'explicit parameter and return constraints keep working';
my $signature = Q[-> Int $x --> Int { 0 }].AST.statements[0].expression.signature;
my &typed_callable = EVAL(RakuAST::PointyBlock.new(:$signature, body => body(7)));
is typed_callable(4), 7, 'a constructed explicit signature binds its argument';
dies-ok { typed_callable('wrong') }, 'a constructed signature keeps its parameter type';
dies-ok { typed_callable() }, 'a constructed required parameter stays required';
my $string_signature = Q[-> --> Str { '' }].AST.statements[0].expression.signature;
my &wrong_return = EVAL(RakuAST::PointyBlock.new(signature => $string_signature, body => body(7)));
dies-ok { wrong_return() }, 'a constructed signature keeps its return constraint';
