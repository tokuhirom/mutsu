use Test;

# Inside a class, a `my constant Atom` passed directly as a call argument is
# the constant, not the nested type `Q::Atom` that shares its short name
# (#11385).

plan 6;

class Q::Atom {}
class Q {
    my constant Atom = 5;
    sub k($x) { $x }
    our sub direct      { k(Atom) }
    our sub parened     { k((Atom)) }
    our sub two-args    { k(Atom), k(1) }
    our sub builtin     { [~] Atom, 1 }
    our sub via-codevar { my $f = &k; $f(Atom) }
    our sub type-arg    { k(Q::Atom) }
}

is Q::direct(), 5, 'a direct call argument is the constant';
is Q::parened(), 5, 'a parenthesized call argument is the constant';
is-deeply Q::two-args(), (5, 1), 'the constant among other arguments';
is Q::builtin(), '51', 'the constant as a builtin argument';
is Q::via-codevar(), 5, 'the constant through a code variable call';
ok Q::type-arg() === Q::Atom, 'the qualified name still reaches the nested type';
