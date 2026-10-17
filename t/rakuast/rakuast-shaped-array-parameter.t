use Test;
use experimental :rakuast;

# Rakudo keeps a synthesized array-shape predicate on shaped parameters, while
# omitting that implementation detail from the constructor-form gist.
plan 10;

sub param($src, $index = 0) {
    $src.AST.statements[$index].expression.signature.parameters.head
}

my $one = param(Q[sub f(@a[2]) { }]);
ok $one.array-shape.defined, 'a shaped array parameter exposes array-shape';
like $one.array-shape.^name, /'RakuAST::Block'/, 'array-shape is a predicate block';
nok $one.gist.contains('array-shape'), 'the constructor gist omits the synthesized predicate';

is EVAL(Q[my $n = 2; sub f(@a[$n]) { @a.elems }; my @a[2] = 1, 2; f(@a)].AST),
    2, 'a one-dimensional dynamic shape is preserved when lowering';
throws-like {
    EVAL(Q[my $n = 2; sub f(@a[$n]) { @a.elems }; my @a[3] = 1, 2, 3; f(@a)].AST)
}, Exception, 'the lowered one-dimensional shape constraint is enforced';

my $two = param(Q[sub f(@a[2, 3]) { }]);
ok $two.array-shape.defined, 'a multidimensional parameter exposes array-shape';
is EVAL(Q[sub f(@a[2, 3]) { @a.elems }; my @a[2; 3] = [1, 2, 3], [4, 5, 6]; f(@a)].AST),
    2, 'multiple dimensions survive RakuAST lowering';
throws-like {
    EVAL(Q[sub f(@a[2, 3]) { @a.elems }; my @a[2; 4] = [1, 2, 3, 4], [5, 6, 7, 8]; f(@a)].AST)
}, Exception, 'every dimension is checked after lowering';

is EVAL(Q[my $n = 2; sub f(@a[$n]) { @a.elems }; my @a[2] = 4, 5; f(@a)].AST),
    2, 'a lexical dimension expression remains live';
ok param(Q[sub f(@a[2, 3]) { }]).array-shape.body.defined,
    'the synthesized shape predicate retains its body';
