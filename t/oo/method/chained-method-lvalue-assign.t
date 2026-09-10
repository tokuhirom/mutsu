use v6;
use Test;

# A method-call lvalue (an rw accessor) must be assignable in EXPRESSION
# position, not only as a bare statement. The middle term of a chained
# assignment (`my $c = $o.a = 42`) and a call argument (`say($o.a = 42)`) are
# the surfacing cases -- previously they died with a `Confused` parse error
# because the expression-level parser left the inner `=` unconsumed.

plan 11;

class C { has $.a is rw }

# Chained declaration RHS: `my $c = C.new.a = 42`
{
    my $c = C.new.a = 42;
    is $c, 42, 'chained decl RHS: my $c = C.new.a = 42';
}

# Chained declaration RHS with a named object, accessor mutated too.
{
    my $o = C.new;
    my $c = $o.a = 42;
    is $c, 42, 'chained decl RHS value';
    is $o.a, 42, 'chained decl RHS mutates the accessor';
}

# Call-argument position: `say(...)` / any listop arg.
{
    my $o = C.new;
    my $v = do { my $r; $r = ($o.a = 7); $r };
    is $v, 7, 'accessor assign as an rvalue in a block';
    is $o.a, 7, 'accessor assign in rvalue mutates';
}

# Non-declaration chained assignment: `$x = $o.a = 42`.
{
    my $o = C.new;
    my $x;
    $x = $o.a = 5;
    is $x, 5, 'plain chained assign value';
    is $o.a, 5, 'plain chained assign mutates accessor';
}

# Two rw accessors chained: `$o.a = $o.b = 7`.
{
    class D { has $.a is rw; has $.b is rw }
    my $o = D.new;
    $o.a = $o.b = 9;
    is $o.a, 9, 'chained accessor-to-accessor (outer)';
    is $o.b, 9, 'chained accessor-to-accessor (inner)';
}

# A `.AT-KEY(k) = v` writeback through a sigilless raw binding (`\h`) as an
# rvalue must mutate the bound container in place (regression: the writeback
# needs the BareWord target's name, like the paren-context lowering).
{
    my %h = a => 42, b => 666;
    for $%h -> \h {
        is (h.AT-KEY("b") = 65), 65, 'AT-KEY assign as rvalue returns the value';
        is h.AT-KEY("b"), 65, 'AT-KEY assign through \h binding persists';
    }
}

done-testing;
