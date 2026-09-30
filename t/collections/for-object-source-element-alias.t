use v6;
use Test;

# #10350: a `for` over an object source aliases the element containers the
# object's iterator yields — an `is Array` subclass's own slots, and the slots
# of an Array whose `.iterator` a class forwards to — rather than binding
# decontainerized copies whose writes are lost.

plan 15;

class Foo is Array {}

{
    my @f is Foo = 1, 2;
    $_ = 5 for @f;
    is-deeply @f.List, (5, 5), 'topic write through an `is Array` variable';
}

{
    my @g := Foo.new(1, 2);
    $_ = 6 for @g;
    is-deeply @g.List, (6, 6), 'topic write through a bound `is Array` instance';
}

{
    my @f is Foo = 1, 2;
    for @f -> $x is rw { $x = 9 }
    is-deeply @f.List, (9, 9), '`is rw` parameter over an `is Array` subclass';
    for @f <-> $x { $x = 8 }
    is-deeply @f.List, (8, 8), '`<->` parameter over an `is Array` subclass';
}

{
    my @f is Foo = 1, 2;
    my @seen;
    for @f -> $v { @f[1] = 7; @seen.push: $v }
    is-deeply @seen, [1, 7], 'a plain `-> $v` still binds the current value';
    throws-like { for @f -> $v { $v = 1 } }, Exception,
        'a plain `-> $v` over an `is Array` subclass stays read-only';
}

class P does Positional {
    has @.x = 1, 2;
    method iterator { @!x.iterator }
    method list { @!x }
}

{
    my @p := P.new;
    $_ = 7 for @p;
    is-deeply @p.x.List, (7, 7), 'topic write through a forwarded `@!x.iterator`';
}

{
    my @a = 1, 2;
    my $x := @a.iterator.pull-one;
    $x = 5;
    is-deeply @a.List, (5, 2), 'Array.iterator.pull-one yields the element container';
}

{
    my @a = 1, 2;
    $_ = 3 for Seq.new(@a.iterator);
    is-deeply @a.List, (3, 3), 'a Seq over an Array iterator aliases its elements';
}

{
    my @a = 1, 2, 3;
    my @b;
    @a.iterator.push-all(@b);
    @b[0] = 9;
    is-deeply @a.List, (1, 2, 3), 'push-all copies values into the target Array';
    my @c;
    @a.iterator.push-exactly(@c, 2);
    @c[0] = 9;
    is-deeply @a.List, (1, 2, 3), 'push-exactly copies values into the target Array';
}

{
    my @a = 1, 2;
    my $it = @a.iterator;
    my $y = $it.pull-one;
    $y = 6;
    is-deeply @a.List, (1, 2), 'assigning a pulled element copies it';
}

# #10356: an element store reaches an `is Array` variable that a closure
# captures, instead of replacing it with a fresh one-element array.
{
    my @f is Foo = 1, 2;
    @f[0] = 9;
    my $c = { @f };
    is-deeply @f.List, (9, 2), 'element store into a captured `is Array` variable';
}

# An expression-position assignment copies an element container that came off
# a call, so re-assigning the variable never writes the source element — the
# shape of a `while $it.pull-one -> \r` loop (Text::CSV's Iterator input).
{
    my @a = [1, 2], [3, 4];
    my $it = @a.iterator;
    my @seen;
    while $it.pull-one -> \r { last if r =:= IterationEnd; @seen.push: r }
    is-deeply @a, [[1, 2], [3, 4]], 'while pull-one -> \r leaves the source intact';
    my @b = 1, 2;
    my $v;
    if ($v = @b.values[1]) { $v = 7 }
    is-deeply @b.List, (1, 2), 'expression assignment of an element copies it';
}
