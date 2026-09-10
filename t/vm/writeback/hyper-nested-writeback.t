use Test;

# A hyper postfix/method mutation (`>>++`, `>>.=method`) on a *nested* element
# (an array element, a hash value, or a scalar bound to a container) must write
# back to that element. Before the fix the runtime only wrote back to top-level
# variable bindings by Arc identity, missing nested elements, so `@b[0]>>++`
# left `@b[0]` unchanged. (Track B element-cell: deep `>>++`.)

plan 13;

# --- Hyper increment on an array element that is itself an array ---
{
    my @b = [1, 2], [3, 4];
    @b[0]>>++;
    is-deeply @b, [[2, 3], [3, 4]], 'hyper ++ on @b[0] writes back to the element';
    @b[1]>>++;
    is-deeply @b, [[2, 3], [4, 5]], 'hyper ++ on @b[1] writes back to the element';
}

# --- Hyper increment on a hash value that is an array ---
{
    my %h = a => [1, 2], b => [3, 4];
    %h<a>>>++;
    is-deeply %h<a>, [2, 3], 'hyper ++ on %h<a> writes back to the value';
    is-deeply %h<b>, [3, 4], 'sibling hash value untouched';
}

# --- Hyper increment through a scalar bound to a whole array ---
{
    my @a = 1, 2, 3;
    my $r := @a;
    $r>>++;
    is-deeply @a, [2, 3, 4], 'hyper ++ through scalar-bound array writes to source';
}

# --- Whole-variable hyper increment still works (regression guard) ---
{
    my @a = 1, 2, 3;
    @a>>++;
    is-deeply @a, [2, 3, 4], 'hyper ++ on whole @-variable still works';
    my %h = a => 1, b => 2;
    %h>>++;
    is-deeply %h, {a => 2, b => 3}, 'hyper ++ on whole %-variable still works';
}

# --- Hyper .=method mutation on a nested element ---
{
    my @b = ["a", "b"], ["c", "d"];
    @b[0]>>.=uc;
    is-deeply @b, [["A", "B"], ["c", "d"]], 'hyper .=uc on @b[0] writes back';
}

# --- deepmap-style nested increment (whole structure) ---
{
    my @a = [1, 2], [3, 4];
    deepmap(++*, @a);
    is-deeply @a, [[2, 3], [4, 5]], 'deepmap(++*) mutates nested elements';
}

# --- A bound element of @b stays shared after hyper mutation ---
{
    my @b = [1, 2], [3, 4];
    my $inner := @b[0];
    @b[0]>>++;
    is-deeply $inner, [2, 3], 'a separate bind to @b[0] sees the hyper mutation';
}

# --- A whole-variable hyper mutation must NOT corrupt a copy ---
# (lvalue-precise writeback: `@a>>++` writes to @a's binding, not to every
# Arc-identity-sharing binding, so the independent copy `@x` is preserved.)
{
    my @a = 1, 2, 3;
    my @x = @a;
    @a>>++;
    is-deeply @x, [1, 2, 3], 'copy is not corrupted by hyper ++ on the source';
    is-deeply @a, [2, 3, 4], 'source is incremented';
}

# --- A whole-container `:=` bound alias DOES see the hyper mutation ---
{
    my @a = 1, 2, 3;
    my $b := @a;
    @a>>++;
    is-deeply $b, [2, 3, 4], 'bound alias sees hyper ++ through the shared cell';
}
