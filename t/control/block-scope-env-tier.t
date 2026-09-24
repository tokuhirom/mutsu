use Test;

# A lexical block runs over an env tier of its own and merges back only the
# names it wrote (#9170). These pin the scoping rules that merge has to keep:
# what propagates out of a bare block, an `if` branch and a `"{ ... }"`
# interpolation block, and what stays behind.

plan 21;

# --- bare block (BlockScope) ---
{
    my $outer = 1;
    my $seen;
    { my $inner = 5; $outer = $outer + $inner; $seen = $inner }
    is $outer, 6, 'bare block: assignment to an outer lexical propagates';
    is $seen, 5, 'bare block: a value read out of a block-local declaration';
}
{
    my $x = 'outer';
    { my $x = 'inner'; }
    is $x, 'outer', 'bare block: a shadowing declaration does not clobber the outer one';
}
{
    my $n = 0;
    for ^3 { { my $y = $_; $n += $y } }
    is $n, 3, 'bare block in a loop: a fresh declaration starts over every iteration';
}
{
    { try die 'boom' }
    is ~$!, 'boom', 'bare block: $! set by a try inside propagates out';
}
{
    { package InBlockPkg { our $v = 42 } }
    is $InBlockPkg::v, 42, 'bare block: a package declared inside stays visible';
}
{
    { my class LexOnly { } }
    throws-like 'LexOnly.new', X::Undeclared::Symbols,
        'bare block: a my class does not outlive its block';
}
{
    my $*dyn = 'outer';
    { my $*dyn = 'inner'; }
    is $*dyn, 'outer', 'bare block: a my $*dyn redeclaration is block-scoped';
    { $*dyn = 'written' }
    is $*dyn, 'written', 'bare block: a write to an existing dynamic propagates';
}

# --- if branch (BlockLocalScope) ---
{
    my $x = 99;
    if True { my $x = 5 }
    is $x, 99, 'if branch: a shadowing declaration does not clobber the outer one';
}
{
    my $acc = '';
    if True { my $tmp = 'a'; $acc ~= $tmp }
    is $acc, 'a', 'if branch: assignment to an outer lexical propagates';
    my $t = 0;
    for ^4 { if $_ %% 2 { my $y = 10; $t += $y } }
    is $t, 20, 'if branch in a loop: declaration and write-through every time';
}
{
    my @outer = 1, 2;
    my $b;
    if True { my $local = 3; $b := @outer }
    is $b.elems, 2, 'if branch: a := binding made to an outer variable survives';
}

# --- "{ ... }" interpolation (DoBlockExpr, scope-isolating) ---
{
    my $x = 1;
    my $s = "a{ $x = 7 }b";
    is $s, 'a7b', 'interpolation block: value';
    is $x, 7, 'interpolation block: assignment to an outer lexical persists';
}
{
    my $s = "<{ my $only-here = 3; $only-here * 2 }>";
    is $s, '<6>', 'interpolation block: a declaration inside is usable there';
}
{
    my $t = 0;
    my $s;
    for ^5 { $s = "a{ $t }b"; $t++ }
    is $s, 'a4b', 'interpolation block in a loop sees the current outer value';
}

# --- blocks inside routines (the enclosing env is itself a scoped frame) ---
{
    sub f($n) {
        my $r = 0;
        { my $k = $n * 2; $r = $k }
        if $n > 0 { my $j = 1; $r += $j }
        "{ $r }"
    }
    is f(3), '7', 'blocks inside a sub merge into the sub frame';
}
{
    # Deep enough to push the env chain past its overlay-depth cap, which
    # makes some blocks take the whole-env fallback path.
    sub deep($n) {
        my $acc = $n;
        { my $d = 1; $acc += $d }
        $n == 0 ?? $acc !! $acc + deep($n - 1)
    }
    is deep(40), 861, 'nested blocks at recursion depth 40';
}

# --- END phaser registered in a block sees the block's final values ---
{
    my $res = '';
    my $p = run $*EXECUTABLE, '-e', '{ my $v = 1; END { print $v }; $v = 2 }', :out;
    $res = $p.out.slurp(:close);
    is $res, '2', 'END in a block captures the final value of a block lexical';
}

# --- a fresh block-local does not leak into a later sibling block ---
{
    my @seen;
    for ^2 {
        { my $once; @seen.push: $once.defined ?? 'leak' !! 'fresh'; $once = 1 }
    }
    is @seen.join(','), 'fresh,fresh', 'a fresh block-local is Nil again on the next entry';
}
