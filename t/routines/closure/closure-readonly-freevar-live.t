use v6;
use Test;

plan 8;

# A read-only free variable must always resolve to the LIVE caller binding,
# even when it is type-constrained (and therefore not boxed into a shared
# ContainerRef cell). The per-closure-instance frozen state must only apply
# to free vars the closure body actually writes.
# (DBIish 36-pg-enum: a converter sub stored in a hash attribute read the
# captured $expected as of the previous call, missing the mainline's later
# write.)

{
    my Str $e = "Yes";
    my $s = sub ($v) { "$v-$e" };
    is $s("a"), "a-Yes", 'typed captured scalar: first call sees initial value';
    $e = "No";
    is $s("b"), "b-No", 'typed captured scalar: later mainline write is visible after a prior call';
}

{
    my Int $n = 1;
    my $s = sub () { $n };
    is $s(), 1, 'typed Int capture: first read';
    $n = 42;
    is $s(), 42, 'typed Int capture: rewrite between calls is visible';
}

# The converter-in-hash-attribute shape from DBIish 36-pg-enum.
{
    class K { has %.c; }
    my $k = K.new;
    my Str $e = "Yes";
    $k.c{"x"} = sub ($v) { "$v-$e" };
    is $k.c{"x"}("a"), "a-Yes", 'stored sub sees initial value';
    $e = "No";
    is $k.c{"x"}("b"), "b-No", 'stored sub sees mainline rewrite';
}

# Per-instance mutable state for WRITTEN free vars still works: two closures
# from the same factory each keep their own counter.
{
    sub make-counter() {
        my $n = 0;
        return sub () { $n = $n + 1; $n };
    }
    my $c1 = make-counter();
    my $c2 = make-counter();
    $c1(); $c1();
    is $c1(), 3, 'factory closure keeps its own accumulated state';
    is $c2(), 1, 'sibling closure instance state is independent';
}
