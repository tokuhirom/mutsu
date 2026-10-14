use v6;
use Test;

# A declaration followed directly by a postfix (`my @a.[2] = 42`,
# `my Int $x.say`, `my @a.push(1)`). Measured on rakudo 2026.09 it is one
# ApplyPostfix whose operand is the VarDeclaration::Simple -- the same node as
# `(my @a)[2] = 42`, which computes the same thing.

plan 12;

sub expr($src) { $src.AST.statements[0].expression }

my $idx = expr(Q[my @a.[2] = 42]);
is $idx.^name, 'RakuAST::ApplyPostfix', 'my @a.[2] = 42 is a postfix application';
is $idx.operand.^name, 'RakuAST::VarDeclaration::Simple', '... on the declaration';
is $idx.postfix.^name, 'RakuAST::Postcircumfix::ArrayIndex', '... of an index';
ok $idx.postfix.assignee.defined, '... that assigns';
my $call = expr(Q[my Int $x.say]);
is $call.operand.^name, 'RakuAST::VarDeclaration::Simple', 'my Int $x.say calls on the declaration';
is $call.postfix.^name, 'RakuAST::Call::Method', '... a method';

# EVAL direction: the declaration lands in the enclosing scope.
is-deeply EVAL(Q[my @a.[2] = 42; @a].AST), [Any, Any, 42], 'EVAL of my @a.[2] = 42';
is-deeply EVAL(Q[my %h.<k> = 1; %h].AST), {k => 1}, 'EVAL of my %h.<k> = 1';
is-deeply EVAL(Q[my @a.push(1); @a].AST), [1], 'EVAL of my @a.push(1)';
is EVAL(Q[my Int $x.raku; $x.^name].AST), 'Int', 'EVAL of my Int $x.method keeps the type';

# Semantics of the parsed forms.
my @b\  .[1] = 3;
is-deeply @b, [Any, 3], 'the unspace form assigns the declared array';
my %g.<z> = 2;
is-deeply %g, {z => 2}, 'a hash index on the declaration';
