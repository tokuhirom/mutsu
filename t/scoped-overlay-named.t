use v6;
use Test;

# Regression pin for the scoped/overlay env on the named (heavy) function path
# (call_compiled_function_named) and the Env *tombstone* mechanism it needs.
# See docs/vm-dual-store.md (Slice 6). A callee's `my $x` clears an outer
# sigilless `\x`'s `__mutsu_sigilless_readonly::x` flag via env.remove(); under a
# scoped overlay that remove must SHADOW the parent's flag (a tombstone), not be
# a no-op — otherwise `$x := ...` wrongly hits the inherited readonly mark and
# dies with "Cannot modify an immutable value".

plan 11;

# --- the exact regression: := in a raw-param sub called from a sigilless
#     for-loop whose loop var shares the bare name of a callee local ---
sub ts($d, Mu \seed, Mu \endpoint, \list) {
    my $result;
    $result := list.List;
    my @out = $result;
    if seed ~~ Array { seed.push(endpoint); @out.push(seed); }
    @out;
}
my @tests = "a", [0, 1], 3, (1, 2, 3),
            "b", [5, 6], 8, (4, 5, 6);
my @seen;
for @tests -> \d, \seed, \endpoint, \result {
    @seen.push(ts(d, seed, endpoint, result).raku);
}
is @seen.elems, 2, 'both iterations ran (no immutable error)';
like @seen[0], /'1, 2, 3'/, 'first := result correct';
like @seen[1], /'4, 5, 6'/, 'second := result correct';

# --- multi / where / slurpy / default (named heavy path) ---
sub f($x, $y = 10, *@rest) { $x + $y + @rest.sum }
is f(1), 11, 'default param';
is f(1, 2, 3, 4), 10, 'slurpy';

sub g($n where * > 0) { $n * 2 }
is g(5), 10, 'where constraint passes';
dies-ok { g(-1) }, 'where constraint rejects';

multi h(Int $x) { "int" }
multi h(Str $x) { "str" }
is h(42), 'int', 'multi Int';
is h("x"), 'str', 'multi Str';

# --- captured outer mutation + rw param through named path ---
my $acc = 0;
sub add($n, $m = 1) { $acc += $n + $m }
add(5); add(10, 2);
is $acc, 18, 'captured outer mutation persists';

sub inc($x is rw) { $x++ }
my $v = 41; inc($v);
is $v, 42, 'rw param writeback';
