use Test;

plan 9;

# `X.=meth` is `X = X.meth`. When the (evaluated-once) target X provides a
# `STORE` method it is a custom container, so the `.=` becomes `X.STORE(X.meth)`
# and the whole expression evaluates to the container X itself (not STORE's
# return value). https://github.com/rakudo/rakudo/commit/7fe23136da

{
    my @log;
    my class Foo {
        method STORE ($x) { @log.push("STORE:$x"); 'store-ret' }
        method foo { 42 }
    }
    my $r = (Foo.new).=foo;
    is-deeply @log, ['STORE:42'], 'STORE called once with the method result';
    isa-ok $r, Foo, '.= on a class instantiation returns the container, not STORE return';
}

{
    my class Bar {
        has $.v is rw;
        method STORE ($x) { $!v = $x; self }
        method double { 21 }
    }
    my $r = (Bar.new).=double;
    isa-ok $r, Bar, 'STORE container result is the instance';
    is $r.v, 21, 'STORE mutated the instance with the method result';
}

# The target is evaluated exactly once (no double construction / double method
# side effects).
{
    my $news = 0;
    my class Baz {
        method STORE ($x) {}
        method foo { 7 }
    }
    sub make { $news++; Baz.new }
    (make).=foo;
    is $news, 1, 'target expression evaluated exactly once';
}

# An object WITHOUT a STORE method keeps the plain method-call value (mutsu's
# historical lax behavior for `.=` on a non-lvalue).
{
    my class NoStore { method foo { 99 } }
    my $r = (NoStore.new).=foo;
    is $r, 99, 'no STORE method => plain method result';
}
{
    is (3.7).Int, 3, 'sanity: non-lvalue method call still works';
}

# `.=` on a real lvalue variable is unaffected (assigns the method result).
{
    my $s = 'abc';
    $s .= uc;
    is $s, 'ABC', '.= on a scalar lvalue assigns the method result';

    my @a = <c a b>;
    @a .= sort;
    is-deeply @a, ['a', 'b', 'c'], '.= on an array lvalue assigns the sorted list';
}
