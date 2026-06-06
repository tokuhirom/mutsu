use Test;

plan 8;

# Hyper operators inherit the precedence of their base operator:
# »*« binds tighter than »+«.
{
    my @r = (1, 2, 3) »+« (10, 20, 30) »*« (2, 3, 4);
    is ~@r, '21 62 123', 'hyper »*« is tighter than »+«';
}
{
    my @r = (1, 2, 3) »*« (10, 20, 30) »+« (2, 3, 4);
    is ~@r, '12 43 94', 'hyper »*« first even when written on the left';
}
{
    my @r = (1, 2, 3) »+« (10, 20, 30) »+« (100, 200, 300);
    is ~@r, '111 222 333', 'same-precedence hyper chain is left-assoc';
}
{
    my @r = (2, 3) »+« (10, 20) »*« (3, 4) »+« (1, 1);
    is ~@r, '33 84', 'mixed hyper precedence chain';
}

# Hyper postfix ++ distributes recursively into nested arrays, mutating in place.
{
    my @r = [1, 2], [3, [4, 5]];
    @r»++;
    is ~@r, '2 3 4 5 6', 'hyper ++ distributes recursively (stringified)';
    is-deeply @r, [[2, 3], [4, [5, 6]]], 'hyper ++ mutates nested arrays in place';
}
{
    my @r = [10, 20], [30, [40, 50]];
    @r>>--;
    is-deeply @r, [[9, 19], [29, [39, 49]]], 'hyper -- distributes recursively (ASCII)';
}

# Hyper method call distributes recursively through nested structures.
{
    my @r = [-1, -2], [-3, [-4, -5]];
    my @abs = @r>>.abs;
    is-deeply @abs, [[1, 2], [3, [4, 5]]], 'hyper .abs distributes recursively';
}
