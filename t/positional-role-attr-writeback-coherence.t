use Test;

# Coherence pin for the env<->locals single-store path
# (MUTSU_NO_BLANKET_RECONCILE): an object subscript assignment
# (`$obj[i] = v`) that dispatches ASSIGN-POS/BIND-POS to a `@`/`%`
# attribute via `handles` must persist the mutation through the caller's
# local slot, not only through the env entry that the (removed) blanket
# reconcile used to pull. Must pass identically with and without the
# blanket reconcile.

plan 7;

# 1. plain Positional role, handles AT-POS/ASSIGN-POS
{
    role P does Positional { has @!c handles <AT-POS ASSIGN-POS>; }
    my $p = P.new;
    $p[0] = 11;
    $p[1] = 22;
    is "$p[0] $p[1]", "11 22", "plain Positional role element assign persists";
}

# 2. parametric Positional role with typed children attribute
{
    role ER[::T] does Positional { has ER[T] @!c handles <AT-POS ASSIGN-POS BIND-POS>; }
    my $e = ER[Int].new;
    $e[0] = ER[Int].new;
    $e[1] = ER[Int].new;
    isa-ok $e[0], ER, "parametric typed child written via ASSIGN-POS reads back";
    isa-ok $e[1], ER, "second parametric typed child reads back";
}

# 3. parametric role keeps sibling scalar attribute across indexed writes
{
    role TreeNode[::T] does Positional {
        has TreeNode[T] @!children handles <AT-POS ASSIGN-POS BIND-POS>;
        has T $.data is rw;
    }
    my $tree = TreeNode[Int].new;
    $tree.data = 3;
    $tree[0] = TreeNode[Int].new;
    $tree[1] = TreeNode[Int].new;
    $tree[0].data = 1;
    $tree[1].data = 4;
    is ($tree.data, $tree[0, 1]>>.data).flat.join(","), "3,1,4",
        "indexed writes preserve other mixin attributes";
}

# 4. BIND-POS via handles persists
{
    role B does Positional { has @!c handles <AT-POS BIND-POS>; }
    my $b = B.new;
    $b[0] := 99;
    is $b[0], 99, "BIND-POS via handles persists";
}

# 5. Associative handles: ASSIGN-KEY persists
{
    role H does Associative { has %!h handles <AT-KEY ASSIGN-KEY>; }
    my $h = H.new;
    $h<a> = 1;
    $h<b> = 2;
    is $h<a>, 1, "Associative ASSIGN-KEY via handles persists (a)";
    is $h<b>, 2, "Associative ASSIGN-KEY via handles persists (b)";
}
