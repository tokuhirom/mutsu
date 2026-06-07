use Test;

# Block-local `my` declarations must be lexically scoped to their block.
# An inner `my $x` shadowing an outer `$x` must NOT clobber the outer slot,
# and the name must not leak out of the block (Slice 3 of lever C / PLAN.md).

plan 24;

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
# `if`/`unless`/`else` branches that declare a block-local `my` are wrapped in a
# BlockLocalScope opcode (Slice 3b): the loop bodies' shadow-only restore
# re-exposes the outer binding on exit, without the full env restore of a
# BlockScope (which would revert `:=` bindings made to outer vars in the branch).
{
    my $x = 99;
    if True { my $x = 5 }
    is $x, 99, 'if-body my does not clobber outer $x';
}

# --- else body: shadow must not clobber outer ---
{
    my $x = 99;
    if False { } else { my $x = 5 }
    is $x, 99, 'else-body my does not clobber outer $x';
}

# --- unless body: shadow must not clobber outer ---
{
    my $x = 99;
    unless False { my $x = 5 }
    is $x, 99, 'unless-body my does not clobber outer $x';
}

# --- if/else both branches declare a shadow ---
{
    my $x = 99;
    if True { my $x = 1 } else { my $x = 2 }
    is $x, 99, 'if/else both-branch my does not clobber outer $x';
}

# --- a `:=` binding to an outer var inside an if-branch must survive ---
{
    my $r;
    if True { my @rr = 10, 20, 30; $r := @rr[0] }
    is $r, 10, 'if-branch := binding to outer var survives BlockLocalScope';
}

# --- closure capturing an if-branch-local must see its frozen value ---
{
    my $cx = 1;
    my $c;
    if True { my $cx = 5; $c = { $cx } }
    is $c.(), 5, 'closure captures if-branch-local value';
    is $cx, 1, 'if-branch-local does not clobber outer captured name';
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

# --- statement-modifier `my` is enclosing-scoped and must survive the loop ---
# (regression guard: the loop-body-local restore must NOT wipe these, since a
# statement modifier introduces no block. See roast S04-statement-modifiers/for.t.)
{
    (my @a).push: $_ for ^3;
    is @a.join(','), '0,1,2', 'stmt-modifier my @a accumulates and survives the loop';
}
{
    my $s;
    $s = $_ for 1..3;
    is $s, 3, 'stmt-modifier my $s survives the loop';
}
# A prior sibling block declaring the same name must not make the restore fire
# (its slot lingers frame-wide but is incoherent with the fresh declaration).
{
    { my @a = (5, 7, 9); }
    (my @a).push: $_ for ^3;
    is @a.join(','), '0,1,2', 'stmt-modifier my @a unaffected by a popped sibling-block @a';
}
# Genuine outer shadow still restores even with a later statement modifier.
{
    my @a = 1, 2, 3;
    for 1..2 { my @a = 7, 8 }
    is @a.join(','), '1,2,3', 'genuine outer @a restored after block-body shadow';
}
