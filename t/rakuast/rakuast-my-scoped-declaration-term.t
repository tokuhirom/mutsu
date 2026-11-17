use experimental :rakuast;
use Test;

# A `my class` written as a term (`my class Foo { }.new`, `(my class :: { })`)
# is a lexical class: its name does not outlive its scope, and RakuAST keeps
# its `my`. A `my rule` keeps its `my` / `our` too, as a `my token` does
# (#14880, the `.DEPARSE` corpus's nan.t, real-bridge.t and unknown.t).

plan 10;

sub deparse($src) { $src.AST.statements[*-1].DEPARSE.subst("\n", ' ', :g).subst(/\s+/, ' ', :g) }

is deparse(q{my $m = my class MyNum is Num { }.new(1)}),
    'my $m = my class MyNum is Num { }.new(1)', 'a my class term keeps its my';
is deparse(q{say (my class :: does Real { method Num { 1e0 } }).Bridge}),
    'say my class :: does Real { method Num { 1e0 } }.Bridge', 'an anonymous my class too';
is deparse(q{say (class :: { }).^name}), 'say anon class :: { }.^name',
    'an anonymous class without my is anon';
is deparse(q{say my class B { }.^name}), 'say my class B { }.^name', 'a named one';
is deparse(q{{ my rule foo { "x" } }}), 'my rule foo { x }', 'a my rule';
is deparse(q{our rule bar { x }}), 'our rule bar { x }', 'an our rule';

{
    my $obj = my class LexTerm { method hi { 'hi' } }.new;
    is $obj.hi, 'hi', 'the class works where it is declared';
}
nok ::('LexTerm'), 'and its name is not seen outside its scope';
is (my class :: { method n { 42 } }).n, 42, 'an anonymous my class works';

my rule lexrule { a b }
ok 'a b' ~~ /<lexrule>/, 'a my rule matches';
