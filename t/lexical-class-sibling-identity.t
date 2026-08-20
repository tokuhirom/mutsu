use v6;
use Test;

plan 6;

# ADR-0047 S2: two SIBLING (non-nested) `my class` blocks of the same name must
# not retarget each other's already-existing instances. Before ADR-0047 P1,
# `pop_lexical_class_scope` released the first block's claim on the bare
# registry key `C`, so the second block's declaration silently clobbered
# `registry.classes["C"]` and `$a.go` started dispatching into the SECOND
# class's methods -- a silent wrong answer, not an error.
{
    my ($a, $b);
    { my class C { method go() { "first" } }; $a = C.new; }
    { my class C { method go() { "second" } }; $b = C.new; }
    is $a.go, "first", "S2: first sibling lexical class keeps its own method body";
    is $b.go, "second", "S2: second sibling lexical class has its own method body";
}

# ADR-0047 S3: an INNER `my class` declared in a nested block must not
# permanently steal the outer name's env binding once the inner block exits.
# Before ADR-0047 P2, `Foo`'s env binding stayed pointed at the inner
# (already out-of-scope) class forever, so a bareword `Foo` after the block
# resolved to the wrong (inner) class even though existing instances of the
# outer class were unaffected (mangling already protected those).
{
    my class Foo { method go() { "outer" } }
    my $o = Foo.new;
    { my class Foo { method go() { "inner" } } }
    is $o.go, "outer", "S3: an escaped outer instance is unaffected by a shadowing inner class";
    is Foo.new.go, "outer", "S3: the outer 'Foo' binding is restored after the inner block exits";
}

# The same shape for `my grammar`, which shares the class registration path
# (`grammar` compiles through `ClassDecl` with an implicit `is Grammar`
# parent) and therefore the same site-key mangling and env-binding restore.
{
    my ($a, $b);
    { my grammar G { token TOP { 'x' } }; $a = G; }
    { my grammar G { token TOP { 'y' } }; $b = G; }
    ok $a.parse('x'), "S2 (grammar): first sibling lexical grammar keeps parsing 'x'";
    ok $b.parse('y'), "S2 (grammar): second sibling lexical grammar parses 'y'";
}

done-testing;
