use Test;

# A lexical package (`my package`, `my class`) is a distinct package per
# declaration, with its own stash. A qualified name (`tmp::Foo`, `$Q::v`)
# resolves its head lexically, to the declaration in scope, not to whichever
# declaration of that spelling bound the member last (#17449, CompUnit::Util's
# `set-in-WHO`).

plan 10;

my @seen;
{
    my package tmp {};
    BEGIN tmp.WHO<Foo> := 'foo';
    @seen.push: tmp::Foo;
}
{
    my package tmp {};
    BEGIN @seen.push: tmp.WHO<Foo>:exists;
    BEGIN { tmp.WHO.package_at_key('Foo'); tmp.WHO<Foo>.WHO<$Baz> := 'bar'; }
    @seen.push: tmp::Foo::<$Baz>;
}
is @seen[0], False, 'a second declaration starts with an empty stash';
is @seen[1], 'foo', 'the first declaration keeps its own member';
is @seen[2], 'bar', 'and the second its own';

my &first;
{ my package tmp {}; BEGIN tmp.WHO<Foo> := 'a'; &first = { tmp::Foo } }
{
    my package tmp {};
    BEGIN tmp.WHO<Foo> := 'b';
    is tmp::Foo, 'b', 'a later declaration reads its member';
}
is first(), 'a', 'a closure reads the declaration it closed over';

{
    my class Q { our $v = 1 }
    {
        my class Q { our $v = 2 }
        is $Q::v, 2, 'an inner lexical class has its own our variable';
    }
    is $Q::v, 1, 'the outer one keeps its own';
    $Q::v = 7;
    is Q::<$v>, 7, 'a write through the qualified name reaches its stash';
}

sub make($n) { my class K { our $c }; $K::c = $n; K }
is make(3).WHO<$c>, 3, 'a lexical class in a routine';

{
    my package tmp {};
    throws-like { EVAL 'tmp::Foo' }, Exception,
        'a member another declaration bound is not visible here';
}
