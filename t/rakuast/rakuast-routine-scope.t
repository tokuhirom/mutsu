use v6;
use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 27;

sub expression-of(Str $source) {
    $source.AST.statements[0].expression
}

sub member-of(Str $source) {
    expression-of($source).body.body.statement-list.statements[0].expression
}

my $body = RakuAST::Blockoid.new(Q[42].AST);
my $name = RakuAST::Name.from-identifier('f');

is expression-of(Q[sub f {}]).scope, 'my', 'a parsed named sub defaults to my';
is expression-of(Q[our sub f {}]).scope, 'our', 'a parsed our sub keeps its scope';
is member-of(Q[class C1 { method f {} }]).scope, 'has', 'a parsed method defaults to has';
is member-of(Q[class C2 { my method f {} }]).scope, 'my', 'a parsed my method keeps its scope';
is member-of(Q[class C3 { our method f {} }]).scope, 'our', 'a parsed our method keeps its scope';
is member-of(Q[class C4 { submethod f {} }]).scope, 'has', 'a parsed submethod defaults to has';
is member-of(Q[class C5 { my submethod f {} }]).scope, 'my', 'a parsed my submethod keeps its scope';
is member-of(Q[class C6 { our submethod f {} }]).scope, 'our', 'a parsed our submethod keeps its scope';

my $sub = RakuAST::Sub.new(name => $name, body => $body);
my $anon-sub = RakuAST::Sub.new(body => $body);
my $method = RakuAST::Method.new(name => $name, body => $body);
my $anon-method = RakuAST::Method.new(body => $body);
my $submethod = RakuAST::Submethod.new(name => $name, body => $body);
my $anon-submethod = RakuAST::Submethod.new(body => $body);
is $sub.scope, 'my', 'a constructed named sub defaults to my';
is $anon-sub.scope, 'anon', 'a constructed anonymous sub defaults to anon';
is $method.scope, 'has', 'a constructed named method defaults to has';
is $anon-method.scope, 'anon', 'a constructed anonymous method defaults to anon';
is $submethod.scope, 'has', 'a constructed named submethod defaults to has';
is $anon-submethod.scope, 'anon', 'a constructed anonymous submethod defaults to anon';

my $explicit-sub = RakuAST::Sub.new(name => $name, scope => 'anon', body => $body);
my $explicit-method = RakuAST::Method.new(name => $name, scope => 'anon', body => $body);
my $explicit-submethod = RakuAST::Submethod.new(name => $name, scope => 'our', body => $body);
is $explicit-sub.scope, 'anon', 'an explicit sub scope wins over the default';
is $explicit-method.scope, 'anon', 'an explicit method scope wins over the default';
is $explicit-submethod.scope, 'our', 'an explicit submethod scope wins over the default';
like $explicit-method.raku, /'scope => "anon"'/, 'a nondefault method scope renders';
unlike $explicit-submethod.raku, /'scope =>'/, 'submethod constructor rendering omits scope';
unlike RakuAST::Sub.new(name => $name, scope => 'my', body => $body).raku,
    /'scope =>'/, 'the named sub default stays implicit in constructor text';
unlike RakuAST::Method.new(name => $name, scope => 'has', body => $body).raku,
    /'scope =>'/, 'the named method default stays implicit in constructor text';

my $parsed = expression-of(Q[sub scope_after_eval_14264 { 42 }]);
is $parsed.scope, 'my', 'parsed scope is available before EVAL';
my &routine = EVAL(RakuAST::StatementList.new(RakuAST::Statement::Expression.new(expression => $parsed)));
is routine(), 42, 'a parsed declaration still lowers and executes';
is $parsed.scope, 'my', 'parsed scope remains available after EVAL';

my $constructed = RakuAST::Sub.new(
    name => RakuAST::Name.from-identifier('constructed_after_eval_14264'),
    body => $body,
);
is $constructed.scope, 'my', 'constructed scope is available before EVAL';
my &constructed-routine = EVAL(RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(expression => $constructed),
));
is constructed-routine(), 42, 'a constructed declaration lowers and executes';
is $constructed.scope, 'my', 'constructed scope stays unchanged after EVAL';
