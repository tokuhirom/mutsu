use experimental :rakuast;
use Test;

# A method call on `self` written with a `.`-twigil variable keeps its
# spelling in RakuAST, as on rakudo's RakuAST frontend: `$.meth(1)`,
# `@.meth(1)`, `%.meth(1)` and `$.meth: 1` are a `Var::Attribute::Public`
# with `args`, and `$.^name` is `self.^name.item` (#14880, the `.DEPARSE`
# corpus's S12-methods/calling_syntax.t). They run as before.

plan 16;

my $n = 0;
sub body-expression($method) {
    $n++;
    my $class = "class AttrCall$n \{ $method \}".AST.statements[0].expression;
    my $meth = $class.body.body.statement-list.statements[0].expression;
    $meth.body.statement-list.statements[0].expression
}

for (q{method m { $.foo(1) }}, '$.foo(1)'),
    (q{method m { @.foo(1) }}, '@.foo(1)'),
    (q{method m { %.foo(1) }}, '%.foo(1)'),
    (q{method m { $.foo: 1 }}, '$.foo(1)') -> ($src, $deparse) {
    my $e = body-expression($src);
    isa-ok $e, RakuAST::Var::Attribute::Public, "$src: a public attribute access";
    is $e.DEPARSE, $deparse, "$src: deparses as $deparse";
}
is body-expression(q{method m { $.foo }}).DEPARSE, '$.foo', 'without arguments it is unchanged';
is body-expression(q{method m { $.^name }}).DEPARSE, 'self.^name.item', '$.^name';

class A {
    method foo($x = 0) { "foo$x" }
    method lst($n) { 1..$n }
    method h($k) { $k => 1 }
    method t1 { $.foo(1) }
    method t2 { @.lst(3) }
    method t3 { %.h('a') }
    method t4 { $.foo: 5 }
    method t5 { $.^name }
}
my $a = A.new;
is $a.t1, 'foo1', '$.foo(1) runs';
is-deeply $a.t2.List, (1, 2, 3), '@.lst(3) runs';
is-deeply $a.t3, {a => 1}, '%.h(...) runs';
is $a.t4, 'foo5', '$.foo: 5 runs';
is $a.t5, 'A', '$.^name runs';
is EVAL(q{class B { method foo($x) { "b$x" }; method t { $.foo(7) ~ $.^name } }; B.new.t}.AST),
    'b7B', 'the view runs through EVAL';
