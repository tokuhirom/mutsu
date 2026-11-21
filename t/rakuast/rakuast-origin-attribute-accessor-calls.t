use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the `ArgList` of a public
# attribute accessor call (`$.foo(1)`, `$.foo: 5`) runs over its arguments,
# and `$.^name` (built as `(self.^name).item`) places its `Call::MetaMethod`
# over the name with an empty `ArgList` at the end.

plan 4;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) if $class ~~ /Var|ArgList|Call|Literal|ApplyPostfix/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[class C { method t { $.foo(1) } }]),
    'Var::Attribute::Public@21-29 ArgList@27-28 IntLiteral@27-28',
    'a parenthesized accessor call';
is placed(Q[class D { method t { @.lst(3, 4) } }]),
    'Var::Attribute::Public@21-32 ArgList@27-31 IntLiteral@27-28 IntLiteral@30-31',
    'an array accessor with two arguments';
is placed(Q[class E { method t { $.foo: 5 } }]),
    'Var::Attribute::Public@21-29 ArgList@28-29 IntLiteral@28-29',
    'a colon accessor call';
is placed(Q[class F { method t { $.^name } }]),
    'ApplyPostfix@21-28 Call::MetaMethod@24-28 ArgList@28-28',
    'a meta-method through the accessor sigil';
