use Test;

# An optional positional (`$x?`) and the built-in parameter traits
# (`is copy` / `is rw` / `is raw` / `is readonly`) across the RakuAST
# boundary (ADR-10723 Stage 1). Measured on rakudo 2026.09: `$x?` is
# `optional => True`, a trait is `traits => (Trait::Is(name => …),)` after
# every other field. This file passes under both mutsu and raku.

plan 14;

sub params(Str $src) { $src.AST.statements[0].expression.signature.parameters }

{
    my @p = params(Q[sub f($y, $x?) { }]);
    ok @p[1].optional, q[$x? is optional];
    nok @p[0].optional, q[a plain positional is not];
}

{
    my @p = params(Q[sub f($x is copy, $y is rw) { }]);
    isa-ok @p[0].traits[0], RakuAST::Trait::Is, 'is copy is a Trait::Is';
    like @p[0].traits[0].gist, /'from-identifier("copy")'/, 'naming copy';
    like @p[1].traits[0].gist, /'from-identifier("rw")'/, 'is rw names rw';
    like @p[0].gist, /'optional => False,' \s* 'traits'/, 'traits follow every other field';
}

# Write direction.
is EVAL(Q[sub f($x is copy) { $x++; $x }; f(1)].AST), 2, 'is copy round-trips';
{
    my $v = 1;
    EVAL Q[sub g($y is rw) { $y = 9 }; g($v)].AST;
    is $v, 9, 'is rw round-trips and writes back';
}
is EVAL(Q[sub h($z?) { $z.defined }; h()].AST), False, '$z? round-trips as optional';
is EVAL(Q[sub h($z?) { $z }; h(4)].AST), 4, 'and still binds an argument';
is EVAL(Q[sub r($q is raw) { $q }; r(5)].AST), 5, 'is raw round-trips';
is EVAL(Q[sub ro($w is readonly) { $w }; ro(3)].AST), 3, 'is readonly round-trips';
is EVAL(Q[my $c = -> $p? { $p // "none" }; $c()].AST), 'none', 'an optional pointy parameter';
is EVAL(Q[my $d = -> $q is copy { $q++; $q }; $d(1)].AST), 2, 'a pointy parameter with a trait';
