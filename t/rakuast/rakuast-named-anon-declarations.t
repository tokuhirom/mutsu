use v6;
use Test;

# A named `anon` declaration is built but not installed under its name.
# Measured on rakudo 2026.09, `anon sub g { }`, `anon class Foo { }`,
# `anon role R { }` and `anon grammar G { }` are the ordinary Sub / Class /
# Role / Grammar nodes with `scope => "anon"` before the name; a nameless
# `anon class { }` has the scope and no name.

plan 12;

sub expression-of(Str $code, Int $i = 0) {
    $code.AST.statements[$i].expression
}

my $sub = expression-of(Q[my &f = anon sub g($x) { $x }]).initializer.expression;
is $sub.^name, 'RakuAST::Sub', 'anon sub is a Sub';
is $sub.scope, 'anon', '... with scope anon';
is $sub.name.canonicalize, 'g', '... and its written name';

my $class = expression-of(Q[my $c = anon class Foo7 { }]).initializer.expression;
is $class.scope, 'anon', 'anon class has scope anon';
is $class.name.canonicalize, 'Foo7', '... and its written name';

like expression-of(Q[anon role R7 { }]).gist, /'scope => "anon",' \s+ 'name  => '/,
    'anon role renders scope before the name';
like expression-of(Q[anon grammar G7 { }]).gist, /'RakuAST::Grammar.new(' \s+ 'scope => "anon"'/,
    'anon grammar has scope anon';
unlike expression-of(Q[anon class { }]).gist, /'name'/, 'a nameless anon class renders no name';

# EVAL of the trees builds the values without installing the names.
my &f = EVAL Q[anon sub g($x) { $x * 2 }].AST;
is f(21), 42, 'EVAL of an anon sub gives the routine';
is &f.name, 'g', '... which keeps its name';
my $c = EVAL Q[anon class Foo8 { method m { 7 } }].AST;
is $c.m, 7, 'EVAL of an anon class gives the type object';
lives-ok { EVAL Q[sub h { }; anon sub h() { }; 1].AST },
    'an anon sub of an existing name redeclares nothing';
