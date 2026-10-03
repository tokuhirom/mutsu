use Test;

# Attribute type smileys, `is default(…)`, `is built` and `my role` in
# RakuAST, measured on rakudo 2026.09.

plan 16;

sub attr($src) {
    $src.AST.statements.head.expression.body.body.statement-list.statements.head.expression
}

my $d = attr(Q|class C1 { has Int:D $.x = 1 }|);
isa-ok $d.type, RakuAST::Type::Definedness, 'a :D attribute type is a Type::Definedness';
is-deeply $d.type.definite, True, 'which is definite';
is-deeply attr(Q|class C2 { has Str:U $.u }|).type.definite, False, 'a :U one is not';

my $default = attr(Q|class C3 { has $.y is default(3) }|);
is $default.traits.elems, 1, '`is default(3)` is one trait';
is $default.traits[0].name.gist, 'RakuAST::Name.from-identifier("default")', 'named default';
isa-ok $default.traits[0].argument, RakuAST::Circumfix::Parentheses, 'with a parenthesized argument';
nok $default.initializer.defined, 'and no initializer of its own';
ok attr(Q|class C4 { has $.z is default(3) = 5 }|).initializer.defined,
    'an explicit initializer beside it is kept';

my $built = attr(Q|class C5 { has $.w is built(False) }|).traits[0];
is $built.name.gist, 'RakuAST::Name.from-identifier("built")', '`is built(False)` is a trait';
isa-ok $built.argument, RakuAST::Circumfix::Parentheses, 'with its argument';
nok attr(Q|class C6 { has $.b is built }|).traits[0].argument.defined, 'a bare `is built` has none';

is Q|my role R7 { }|.AST.statements.head.expression.scope, 'my', '`my role` has scope my';

my $obj = EVAL(Q|class C8 {
    has Int:D $.x = 1;
    has $.y is default(3);
    has $.z is default(3) = 5;
    has @.a is default(0);
    has $.w is built(False) = 'w';
}; C8.new(x => 2, w => 'ignored')|.AST);
is $obj.x, 2, 'a :D attribute survives the round trip';
is "{$obj.y} {$obj.z} {$obj.a[5]}", '3 5 0', '`is default` survives it, with and without an initializer';
is $obj.w, 'w', '`is built(False)` survives it';
is EVAL(Q|my role R9 { method r { 'r' } }; class C9 does R9 { }; C9.new.r|.AST), 'r',
    '`my role` survives it';
