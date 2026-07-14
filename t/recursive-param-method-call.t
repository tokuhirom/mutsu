use Test;

plan 6;

# A method call on a named receiver refreshed the receiver's local slot from
# `env[name]` unconditionally. But a frame's env also carries every same-named
# binding it inherited from its caller (parameters live in local slots, not in
# env), so on an *unchanged* receiver that copied the CALLER's variable over the
# callee's parameter.
#
# A self-recursive routine is exactly that shape — every frame has a `$tree` —
# so any method call on the parameter reverted it to the caller's value, the
# descent never reached a leaf, and the recursion ran until the Rust stack gave
# out. It presented as `fatal runtime error: stack overflow` (roast
# integration/99problems-51-to-60.t P57) but was an infinite recursion.

{
    my @seen;
    sub descend($tree, $d) {
        @seen.push($tree.gist);          # a method call on the parameter
        return if $d >= 2;
        descend($tree[1], $d + 1);
    }
    descend([3, [2, Any, Any], Any], 0);
    is @seen.elems, 3, 'recursive descent through a method-called parameter terminates';
    is @seen[1], '[2 (Any) (Any)]', 'the parameter is the callee argument, not the caller value';
    is @seen[2], '(Any)', '... at every level';
}

{
    # Two reads of the parameter in one expression must agree: the first read
    # used to be right and the second stale.
    my $out;
    sub twice($tree, $d) {
        $out = $tree.WHAT.^name ~ '|' ~ $tree.WHAT.^name if $d == 1;
        return if $d >= 1;
        twice($tree[1], $d + 1);
    }
    twice([3, Any, Any], 0);
    is $out, 'Any|Any', 'both reads of a recursed parameter see the same value';
}

{
    # `not $tree.defined` is the real-world shape: a binary-tree insert that
    # never bottoms out.
    sub add($tree, $node) {
        return [$node, Any, Any] unless $tree.defined;
        return $node <= $tree[0]
            ?? [$tree[0], add($tree[1], $node), $tree[2]]
            !! [$tree[0], $tree[1], add($tree[2], $node)];
    }
    my $t = Any;
    $t = add($t, $_) for 3, 2, 5;
    is $t.gist, '[3 [2 (Any) (Any)] [5 (Any) (Any)]]', 'binary search tree insert bottoms out';
}

{
    # The writeback this gate protects must still happen: a method that really
    # DOES rebind the receiver in env has to refresh the local slot.
    my @a = 1, 2;
    @a.push(3);
    is @a.join(','), '1,2,3', 'a mutating method still updates the receiver';
}

# vim: expandtab shiftwidth=4
