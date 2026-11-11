use Test;
use MONKEY-SEE-NO-EVAL;

# `RakuAST::Class.meta-object` (and Role, Grammar, Package, Module) is the
# type the node declares, usable before `EVAL`; `EVAL` composes that same type
# (ADR-17472, #17472; EEL 0.3's `eel -e` renames inline types before `EVAL`).

plan 16;

{
    my $ast = 'class Bar { has $.x = 4; method hi { "hi" } }; Bar.new.x'.AST;
    my $node = $ast.statements[0].expression;
    my $type = $node.meta-object;
    is $type.^name, 'Bar', 'the meta-object of a class exists before EVAL';
    is $type.new.hi, 'hi', 'and is composed: it has its methods';
    is $type.new.x, 4, 'and its attributes';
    is $ast.EVAL, 4, 'EVAL then runs the unit';
    ok $node.meta-object === $type, 'the meta-object is the same type after EVAL';
}

{
    my $ast = 'class Foo { method m { 42 } }; Foo'.AST;
    my $type = $ast.statements[0].expression.meta-object;
    $type.HOW.set_name($type, 'Renamed');
    my $result = $ast.EVAL;
    is $result.^name, 'Renamed', 'a rename made before EVAL survives it';
    is $result.m, 42, 'the unit refers to the type it declared';
}

{
    my $ast = 'class Evaled { method v { 7 } }; Evaled.v'.AST;
    is $ast.EVAL, 7, 'a class that was never asked for its meta-object';
    my $type = $ast.statements[0].expression.meta-object;
    is $type.^name, 'Evaled', 'answers its meta-object after EVAL';
    is $type.v, 7, 'which is the type the EVAL declared';
}

{
    my $ast = 'role Greets { method greet { "hello" } }; class Greeter does Greets { }; Greeter.new.greet'.AST;
    my $role = $ast.statements[0].expression.meta-object;
    is $role.^name, 'Greets', 'a role meta-object before EVAL';
    my $class = $ast.statements[1].expression.meta-object;
    ok $class.^does($role), 'a class composing it, declared ahead too, does it';
    is $ast.EVAL, 'hello', 'EVAL adopts both';
}

{
    my $ast = 'use Test; class WithPrelude { method t { &ok.name } }; WithPrelude.t'.AST;
    my $type = $ast.statements[1].expression.meta-object;
    is $type.t, 'ok', 'the declaration sees what the unit imported before it';
    is $ast.EVAL, 'ok', 'and the EVAL still runs';
}

{
    my $ast = 'grammar G { token TOP { a+ } }; G.parse("aaa").Str'.AST;
    is $ast.statements[0].expression.meta-object.parse('aa').Str, 'aa',
        'a grammar meta-object parses before EVAL';
}
