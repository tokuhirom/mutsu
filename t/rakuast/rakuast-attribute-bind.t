use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 7;

my $bound_ast = Q[my @source; class Bound { our @.x := @source }].AST;
my $bound = $bound_ast.statements[1].expression.body.body."statement-list"().statements[0].expression;
isa-ok $bound.initializer, RakuAST::Initializer::Bind,
    'a class-level attribute bind has an Initializer::Bind';
is $bound.traits.elems, 0, 'a bind does not add an assignment default trait';

my $assigned_ast = Q[my @source; class Assigned { our @.x = @source }].AST;
my $assigned = $assigned_ast.statements[1].expression.body.body."statement-list"().statements[0].expression;
isa-ok $assigned.initializer, RakuAST::Initializer::Assign,
    'an attribute assignment keeps its own initializer shape';

is-deeply EVAL(Q[my @source = 1, 2; class BoundArray { our @.x := @source }; @source.push(3); BoundArray.x].AST),
    [1, 2, 3], 'a bound array accessor sees later mutations';
is EVAL(Q[my %source = a => 1; class BoundHash { our %.x := %source }; %source<b> = 2; BoundHash.x.elems].AST),
    2, 'a bound hash accessor sees later mutations';
is-deeply EVAL(Q[my @source = 1, 2; class MyBound { my @.x := @source }; @source.push(3); MyBound.x].AST),
    [1, 2, 3], 'a my-scoped attribute binds its source container';
is-deeply EVAL(Q[my @source = 1, 2; class AssignedArray { our @.x = @source }; @source.push(3); AssignedArray.x].AST),
    [1, 2], 'an assigned attribute still copies the source';
