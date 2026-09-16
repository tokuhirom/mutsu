use v6;
use Test;

# GH #8503: `push(@a, ...)` / `@a.push(...)` on an escape-boxed `@` local (a
# captured-and-mutated container ADR-0039 puts behind a shared `ContainerRef`
# cell, e.g. `my Str:D @a` captured by a block passed as a `.map` argument)
# silently severed the cell instead of writing through it.
#
# `append`/`unshift`/`prepend`/`pop` already resolve their target through
# `env_root_descended_mut`, which descends through a `ContainerRef` cell to
# mutate its held Array in place. `push` alone went through a *different*
# helper, `push_to_shared_var`, whose non-shared fallback checked
# `self.env.get(key)` directly -- which never matches `ValueView::Array` when
# the slot holds a cell, so it fell through to a "rebuild a detached array
# and overwrite env[key]" path. That replaced the CELL with a plain array in
# the current frame only; every other holder of the cell (in particular an
# eager `.map`/`.grep` loop's own saved-env snapshot, restored once the loop
# exits) still saw the stale, pre-push cell contents, so the pushed elements
# vanished the moment the loop finished.
#
# Repro (originally `Config::TOML::Dumper` rendering every array as empty --
# https://github.com/tokuhirom/mutsu/issues/7539):
#
#     sub f($l) {
#         my Str:D @elements;
#         $l.map({ push(@elements, "x") });
#         @elements.join(', ');
#     }
#     say f((1, 2, 3));   # raku: x, x, x   mutsu was: (empty)

plan 7;

# --- the issue's own repro: `push(...)` sub form -------------------------
{
    sub f($l) {
        my Str:D @elements;
        $l.map({ push(@elements, "x") });
        @elements.join(', ');
    }
    is f((1, 2, 3)), 'x, x, x', 'push() sub form inside a .map argument mutates the outer typed array';
}

# --- the `.push` method form fails identically per the issue -------------
{
    sub f($l) {
        my Str:D @elements;
        $l.map({ @elements.push("x") });
        @elements.join(', ');
    }
    is f((1, 2, 3)), 'x, x, x', '.push() method form inside a .map argument mutates the outer typed array';
}

# --- an untyped array must keep working (it never took the cell path) ----
{
    sub f($l) {
        my @elements;
        $l.map({ push(@elements, "x") });
        @elements.join(', ');
    }
    is f((1, 2, 3)), 'x, x, x', 'the untyped baseline is unchanged';
}

# --- a `for` loop body (no closure capture, no save/restore) must keep working --
{
    sub f() {
        my Str:D @elements;
        for (1, 2, 3) {
            push(@elements, "x");
        }
        @elements.join(', ');
    }
    is f(), 'x, x, x', 'a for-loop body still mutates the typed array directly';
}

# --- multiple map iterations must accumulate, not just the last one ------
{
    sub f($l) {
        my Str:D @elements;
        $l.map({ push(@elements, $_.Str) });
        @elements.join(',');
    }
    is f((1, 2, 3)), '1,2,3', 'each iteration is pushed, not just the last';
}

# --- the cell identity survives the push (write-through, not detach) -----
{
    my Str:D @a;
    my $before = @a.WHERE;
    my $blk = { push(@a, "x") };
    $blk.();
    is @a.WHERE, $before, 'push writes through the container cell instead of replacing it';
}

# --- .grep exercises the same eager inline-loop merge as .map ------------
{
    sub f($l) {
        my Str:D @elements;
        $l.grep({ push(@elements, "x"); True });
        @elements.join(', ');
    }
    is f((1, 2, 3)), 'x, x, x', '.grep argument closures see the same fix';
}

