use Test;

# Block-local `my` declarations must be lexically scoped to their block.
# An inner `my $x` shadowing an outer `$x` must NOT clobber the outer slot,
# and the name must not leak out of the block (Slice 3 of lever C / PLAN.md).

plan 15;

# --- for loop body: shadow must not clobber outer ---
{
    my $x = 99;
    for 1..3 { my $x = $_ }
    is $x, 99, 'for-body my does not clobber outer $x';
}

# --- while loop body: shadow must not clobber outer ---
{
    my $x = 99;
    my $i = 0;
    while $i < 3 { my $x = $i; $i++ }
    is $x, 99, 'while-body my does not clobber outer $x';
}

# --- C-style loop body: shadow must not clobber outer ---
{
    my $x = 99;
    loop (my $j = 0; $j < 3; $j++) { my $x = $j }
    is $x, 99, 'C-style loop-body my does not clobber outer $x';
}

# --- if body: shadow must not clobber outer ---
# NOTE: `if`/`else` bodies are compiled inline without a runtime loop scope, so
# the loop_local_saved_env restore does not apply. Fixing the if-body clobber
# needs a dedicated block-local-scope mechanism (a BlockScope wrap reverts `:=`
# bindings to outer vars made inside the branch). Deferred — tracked as todo.
{
    my $x = 99;
    if True { my $x = 5 }
    todo 'if-body my clobber needs a block-local-scope mechanism (see PLAN.md)';
    is $x, 99, 'if-body my does not clobber outer $x';
}

# --- else body: shadow must not clobber outer ---
{
    my $x = 99;
    if False { } else { my $x = 5 }
    todo 'else-body my clobber needs a block-local-scope mechanism (see PLAN.md)';
    is $x, 99, 'else-body my does not clobber outer $x';
}

# --- nested blocks ---
{
    my $x = 1;
    for 1..2 {
        my $x = 10;
        for 1..2 { my $x = 100 }
        is $x, 10, 'inner loop my does not clobber middle $x';
    }
    is $x, 1, 'nested loop my does not clobber outermost $x';
}

# --- repeat loop body ---
{
    my $x = 99;
    my $k = 0;
    repeat { my $x = $k; $k++ } while $k < 3;
    is $x, 99, 'repeat-body my does not clobber outer $x';
}

# --- bare block (already works, regression guard) ---
{
    my $x = 99;
    { my $x = 5 }
    is $x, 99, 'bare-block my does not clobber outer $x';
}

# --- outer modified inside loop still works (no false shadow) ---
{
    my $x = 0;
    for 1..3 { $x = $x + $_ }
    is $x, 6, 'plain assignment to outer $x inside loop still works';
}

# --- @ array shadow ---
{
    my @a = 1, 2, 3;
    for 1..2 { my @a = 7, 8 }
    is @a.join(','), '1,2,3', 'for-body my @a does not clobber outer @a';
}

# --- % hash shadow ---
{
    my %h = a => 1;
    for 1..2 { my %h = b => 2 }
    is %h<a>, 1, 'for-body my %h does not clobber outer %h';
}

# --- shadow value is fresh each iteration (closure capture sanity) ---
{
    my $x = 99;
    my @c;
    for 1..3 -> $i { my $x = $i; @c.push({ $x }) }
    is @c.map(*.()).join(','), '1,2,3', 'per-iteration my captured correctly';
    is $x, 99, 'closure-capturing loop does not clobber outer $x';
}
