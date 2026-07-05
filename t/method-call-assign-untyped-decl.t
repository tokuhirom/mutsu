use Test;

# `my @a .= new(...)` desugars to `@a = @a.new(...)`: the invocant is the
# variable, not a type named `@a`. mutsu built a `@a`-named BareWord target,
# which mis-dispatched (`my @c[2;2] .= new(:shape(2,2), ...)` built only a
# 1-element array instead of the 2x2 shaped array).

plan 8;

{
    my @c[2;2] .= new(:shape(2,2), <a b>, <c d>);
    is @c.elems, 2, 'shaped .= new populates all rows';
    is @c[0;1], 'b', 'row 0 col 1';
    is @c[1;0], 'c', 'row 1 col 0';
}
{
    my @a .= new(1, 2, 3);
    is-deeply @a, [1, 2, 3], 'plain array .= new';
}
# Scalars / hashes / typed / class decls are unaffected
{ my $x .= new; is $x.WHAT.gist, '(Any)', 'untyped scalar .= new is Any.new'; }
{ my %h .= new; is %h.WHAT.gist, '(Hash)', 'untyped hash .= new is Hash.new'; }
{ my Int @x .= new; is @x.WHAT.gist, '(Array[Int])', 'typed array .= new uses Array[Int]'; }
{
    class C { has $.n }
    my C $o .= new(n => 5);
    is $o.n, 5, 'typed class .= new';
}
