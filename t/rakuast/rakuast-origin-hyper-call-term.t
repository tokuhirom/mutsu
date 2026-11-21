use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the `Call::Term` of a hyper
# call on a term (`@a».(3)`) runs from its `(` to its `)`, and its `ArgList`
# over the arguments (empty, just inside the `(`, when there are none).

plan 3;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) if $class ~~ /Call|ArgList|MetaPostfix|IntLiteral/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[my @a; @a».(3)]),
    'MetaPostfix::Hyper@9-14 Call::Term@11-14 ArgList@12-13 IntLiteral@12-13',
    'a hyper call with one argument';
is placed(Q[my @a; @a>>.(1, 2)]),
    'MetaPostfix::Hyper@9-18 Call::Term@12-18 ArgList@13-17 IntLiteral@13-14 IntLiteral@16-17',
    'the ASCII marker with two arguments';
is placed(Q[my @a; @a».()]),
    'MetaPostfix::Hyper@9-13 Call::Term@11-13 ArgList@12-12',
    'no arguments';
