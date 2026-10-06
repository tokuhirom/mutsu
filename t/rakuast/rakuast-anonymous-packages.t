use Test;

# Anonymous `class`, `role` and `grammar` expressions in RakuAST, measured on
# rakudo 2026.09: the node a declaration has, with no `name`. The empty name
# spelling (`class :: { }`) is an `anon` scope over the name `::`. EVAL of the
# tree declares the type as the parsed expression does.

plan 27;

sub expr($src) { $src.AST.statements.head.expression }

my $class = expr(Q[class { method m { 1 } }]);
isa-ok $class, RakuAST::Class, '`class { }` is a Class node';
nok $class.name.defined, 'it has no name';
isa-ok $class.body, RakuAST::Block, 'its body is a Block';

my $assigned = expr(Q[my $c = class { }]);
isa-ok $assigned.initializer.expression, RakuAST::Class, 'as an initializer';

my $invoked = expr(Q[class { has $.x }.new]);
isa-ok $invoked.operand, RakuAST::Class, 'as the invocant of a method call';

my $named = expr(Q[class Named { }.new]).operand;
isa-ok $named, RakuAST::Class, 'a named class expression is the same node';
is $named.name.canonicalize, 'Named', 'with its name';

my $role = expr(Q[role { method m { 1 } }]);
isa-ok $role, RakuAST::Role, '`role { }` is a Role node';
nok $role.name.defined, 'it has no name';

my $grammar = expr(Q[grammar { token TOP { a } }]);
isa-ok $grammar, RakuAST::Grammar, '`grammar { }` is a Grammar node';
nok $grammar.name.defined, 'it has no name';

my $empty = expr(Q[class :: is Exception { }]);
is $empty.scope, 'anon', '`class ::` has an anon scope';
isa-ok $empty.name, RakuAST::Name, 'over the name `::`';
is $empty.traits.elems, 1, 'and keeps its `is` clause';
is expr(Q[role :: { }]).scope, 'anon', '`role ::` does too';
is expr(Q[grammar :: { token TOP { a } }]).scope, 'anon', 'and `grammar ::`';

my $does = expr(Q[class :: does Positional { }]);
isa-ok $does.traits[0], RakuAST::Trait::Does, 'a `does` clause is a Trait::Does';
is $does.body.body.statement-list.statements.elems, 0,
    'and not also a statement of the body';

sub run($src) { EVAL($src.AST) }
is run(Q[my $c = class { method m { 42 } }; $c.new.m]), 42, 'an anonymous class declares its methods';
is run(Q[class { has $.x }.new(x => 5).x]), 5, 'and its attributes';
is run(Q[class :: is Exception { method message { "boom" } }.new.message]), 'boom',
    '`class :: is Parent` inherits';
ok run(Q[class :: does Positional { }.new ~~ Positional]), '`class :: does Role` composes the role';
ok run(Q[class Composed does Positional { }.new ~~ Positional]), 'a named class expression composes it too';
is run(Q[(1 but role { method m { 7 } }).m]), 7, 'an anonymous role mixes in';
is run(Q[grammar { token TOP { a+ } }.parse("aaa").Str]), 'aaa', 'an anonymous grammar parses';
is run(Q[my $a = class { }; my $b = class { }; ($a.^name ne $b.^name).Str]), 'True',
    'two anonymous classes are two types';
is run(Q[my $c = class Declared { method m { 1 } }; Declared.new.m]), 1,
    'a named class expression also declares its name';
