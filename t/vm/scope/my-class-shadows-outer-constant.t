use Test;

# A lexical type declaration (`my class`, `my role`, `my grammar`) shadows a
# same-named sigil-less constant of an enclosing scope for the rest of its
# block — in the block itself, in closures created there, and in the type's
# own methods — and the constant is visible again once the block exits
# (#11517).

plan 16;

constant RIS = Int;
my &esc;
{
    my class RIS { method hi { 'class' }; method me { RIS.^name } }
    is RIS.^name, 'RIS', 'the bareword names the inner class';
    is RIS.hi, 'class', 'a method call on the bareword reaches the class';
    is RIS.new.me, 'RIS', 'the class\'s own method sees the class';
    is (-> { RIS.hi })(), 'class', 'a closure in the block sees the class';
    &esc = { RIS.hi };
    {
        is RIS.^name, 'RIS', 'a nested block still sees the class';
        constant RIS = 42;
        is RIS, 42, 'a nested constant shadows the class in turn';
    }
    is RIS.^name, 'RIS', 'the class is back after the nested constant\'s block';
}
is esc(), 'class', 'an escaped closure keeps the class';
is RIS.^name, 'Int', 'the constant is visible again after the block';

constant RR = 5;
{
    my role RR { method r { 'role' } }
    is RR.^name, 'RR', 'a my role shadows an outer constant';
    is (1 but RR).r, 'role', 'and mixes in';
}
is RR, 5, 'the constant is back after the role\'s block';

sub f {
    constant G = Str;
    my @r;
    {
        my grammar G { token TOP { a } }
        @r.push: G.parse('a').Bool;
        @r.push: G.^name;
    }
    @r.push: G.^name;
    @r
}
is-deeply f(), [True, 'G', 'Str'], 'a my grammar shadows a routine\'s constant';

{
    my $s = sub { { my class RIS { }; RIS.^name } };
    is $s(), 'RIS', 'inside a closure, a my class shadows the file constant';
}

{
    my class Plain { method n { Plain.^name } }
    is Plain.new.n, 'Plain', 'a my class with no shadowed constant is unaffected';
}
is RIS.^name, 'Int', 'the file constant is unaffected at the end';
