use Test;

# A `*-1`-style subscript only ever resolved against its container when it was
# the LAST level of a chain. At any earlier level the unresolved WhateverCode
# was stringified straight into a key, so the write landed in a garbage slot and
# was silently lost. Every assertion below also passes under rakudo.

plan 18;

# --- two-level chain, whatever at the inner level ---
{
    my @a = ({},);
    @a[*-1]<x> = 'A';
    is @a[0]<x>, 'A', 'array[*-1]<key> = v writes through';
    is @a.elems, 1, 'and does not grow the array';
}

{
    my @a = ({},);
    @a[*-1]{'x'} = 'A';
    is @a[0]<x>, 'A', 'array[*-1]{key} = v writes through';
}

{
    my @b = ([1, 2],);
    @b[*-1][0] = 9;
    is @b[0][0], 9, 'array[*-1][idx] = v writes through';
    is @b[0][1], 2, 'and leaves the sibling element alone';
}

{
    my @a = ({}, {});
    @a[*-2]<z> = 'Z';
    is @a[0]<z>, 'Z', '*-2 resolves against the container length too';
    nok @a[1]<z>.defined, 'and does not touch the last element';
}

# --- three-level chains ---
{
    my @a = ({ h => {} },);
    @a[*-1]<h><y> = 'A';
    is @a[0]<h><y>, 'A', 'whatever at the first of three levels';
}

{
    my @b = ([[0],],);
    @b[*-1][0][0] = 9;
    is @b[0][0][0], 9, 'whatever ahead of two positional levels';
}

{
    my %m = (k => [{},]);
    %m<k>[*-1]<x> = 'M';
    is %m<k>[0]<x>, 'M', 'whatever in the MIDDLE of a three-level chain';
}

# --- read-modify-write through the same shape ---
{
    my @c = ({},);
    @c[*-1]<x>++;
    is @c[0]<x>, 1, 'postfix ++ through a whatever level';
}

{
    my @d = ({ n => 1 },);
    @d[*-1]<n> += 5;
    is @d[0]<n>, 6, 'metaassign += through a whatever level';
}

# --- the shape that found this: a scope stack held in an attribute ---
{
    class Ctx {
        has @.scope-stack;
        method set(Str:D $name, $value) { @!scope-stack[*-1]{$name} = $value }
        method push-scope(%vars = {}) { @!scope-stack.push: %vars }
        method pop-scope() { @!scope-stack.pop if @!scope-stack.elems > 1 }
        method resolve(Str:D $name) {
            for @!scope-stack.reverse -> %scope {
                return %scope{$name} if %scope{$name}:exists;
            }
            'UNDEF';
        }
    }

    my $c = Ctx.new;
    $c.push-scope({});
    my @seen;
    for <a b c> -> $item {
        $c.push-scope({});
        $c.set('x', $item);
        @seen.push: $c.resolve('x');
        $c.pop-scope;
    }
    is @seen.join(','), 'a,b,c', 'a per-iteration scope pushed onto an attribute array';
    is $c.resolve('x'), 'UNDEF', 'and each scope really was popped';
}

# --- levels that already worked must keep working ---
{
    my @a = (1, 2, 3);
    @a[*-1] = 99;
    is @a[2], 99, 'a single-level whatever subscript still assigns';
}

{
    my @a = ([1, 2],);
    @a[0][*-1] = 9;
    is @a[0][1], 9, 'a whatever at the LAST level still assigns';
}

{
    my %h = (k => [1, 2]);
    %h<k>[*-1] = 9;
    is %h<k>[1], 9, 'whatever at the last level below a hash key';
}

{
    my @a = ({ y => 1 },);
    is @a[*-1]<y>, 1, 'reading through a whatever level was never broken';
}
