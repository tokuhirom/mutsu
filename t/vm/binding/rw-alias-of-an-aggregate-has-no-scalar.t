use Test;

# Raku gives an `@`/`%` variable no Scalar container of its own: a sigilless
# alias of one (`\c` bound to `%a`) IS that aggregate, so `c = {...}` is
# `%a.STORE(...)`, a write every holder sees, and `return-rw c` hands the caller
# the aggregate itself.
#
# mutsu boxed such an alias into a fresh scalar cell at the `return-rw` site,
# modelling a container Raku does not have: the caller then assigned into a cell
# owned by a frame that had already returned, and the write reached nobody. One
# hop happened to work; two did not. Every `Crane::In.in(container, @path) = $value`
# descent is this shape.

plan 14;

sub leaf(\c)  is rw { return-rw c }
sub hop(\c)   is rw { return-rw leaf(c) }

{
    my %a = :k(1);
    my %alias := %a;
    hop(%a) = {:z(9)};
    is-deeply %a, {:z(9)}, 'a whole-hash store through two rw hops reaches the caller';
    is-deeply %alias, {:z(9)}, 'and every other holder of the container';
}

{
    my @b = 1, 2;
    my @alias := @b;
    hop(@b) = [9];
    is-deeply @b, [9], 'the same for an array';
    is-deeply @alias, [9], 'and its aliases';
}

{
    # A `$` scalar is genuinely a container: the cell route is right there.
    my $s = 1;
    hop($s) = 9;
    is $s, 9, 'a scalar still rebinds through the cell';
}

{
    # Element writes through the alias chain were already fine; keep them so.
    my %c = :k(1);
    sub probe(\c) is rw { c<probe> = 7; return-rw leaf(c) }
    probe(%c);
    is %c<probe>, 7, 'an element write through the alias reaches the caller';
}

# An intermediate hop with a slurpy, an extra array parameter, or a `where`-
# constrained multi is the Crane::In shape; none of them may lose the write.
sub hop-slurpy(\c, *@s)  is rw { return-rw leaf(c) }
sub hop-array(\c, @s)    is rw { return-rw leaf(c) }
multi sub leaf-multi(Associative:D \c, @s where { .elems == 0 }) is rw { return-rw c }
sub hop-multi(\c, *@s)   is rw { return-rw leaf-multi(c, @s) }

{
    my %d = :k(1);
    hop-slurpy(%d) = {:z(9)};
    is-deeply %d, {:z(9)}, 'through a slurpy-carrying hop';
}
{
    my %e = :k(1);
    my @empty;
    hop-array(%e, @empty) = {:z(9)};
    is-deeply %e, {:z(9)}, 'through a hop with an array parameter';
}
{
    my %f = :k(1);
    hop-multi(%f) = {:z(9)};
    is-deeply %f, {:z(9)}, 'through a `where`-constrained multi';
}

# An rw METHOD whose tail names an `@`/`%` container hands back the aggregate,
# not a cell -- that is still an lvalue.
class In { method in(\c, *@s) is rw { return-rw leaf-multi(c, @s) } }
{
    my %g = :k(1);
    In.in(%g) = {:z(9)};
    is-deeply %g, {:z(9)}, 'an rw class method is an lvalue for an aggregate too';
}

# A SIGILLESS argument to an lvalue call names a container just as a `$`-sigiled
# one does. Without the tag the callee's `\c` got a bare value with no source
# name, was marked a readonly non-lvalue, and the assignment died with
# "Cannot modify an immutable Int (0)".
{
    my $a = 0;
    sub write-through(\c) { leaf(c) = 1 }
    write-through($a);
    is $a, 1, 'an lvalue call taking a sigilless alias writes through';
}
multi sub leaf-any(\c, @s where { .elems == 0 }) is rw { return-rw c }
class InAny { method in(\c, *@s) is rw { return-rw leaf-any(c, @s) } }
{
    my $b = 0;
    sub write-method(\c) { InAny.in(c) = 2 }
    write-method($b);
    is $b, 2, 'the same through an rw method';
}
{
    my $c = 0;
    sub write-nested(\c, :@path!, :$value!) { InAny.in(c, @path) = $value; c }
    write-nested($c, :path(), :value(3));
    is $c, 3, 'and with the path/value shape Crane.set uses';
}

# A sigilless parameter's DECLARED type is checked once, when the argument
# binds; a later write through the alias goes into the caller's container and is
# checked against that container's own constraint. mutsu registered the
# parameter's type in the assignment-time lane instead, so
# `sub h(Associative \c) { c = Empty }` died with "expected Associative but got
# Slip" where Rakudo stores the Slip into the caller's untyped scalar. That is
# Crane's `remove-from-associative(\container, :in-place)`.
{
    sub empty-it(Associative \container) { container = Empty; container }
    my $root = {:a(1)};
    is empty-it($root).raku, 'Empty', 'a typed sigilless alias may be assigned a Slip';
}
