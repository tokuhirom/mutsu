# A `:=` rebind whose source is a SUBSCRIPT must not adopt a container from an
# ancestor call frame.
#
# The compiler tags a `$x := <subscript>` source with a synthetic per-site name
# `__mutsu_bind_index_ref_N`. That tag denotes nothing of its own -- the value
# under it is the oracle -- and N is a constant-pool index, so two unrelated
# routines routinely mint the SAME name. Call paths that chain the callee's env
# onto the caller's then let a lookup under that name find a CALLER's temp, and
# the bind's container-promotion adopted that stale cell: `$root := $root{$k}`
# kept naming the container the key was looked up in instead of the deferred
# entry for the missing key.
#
# A recursive path-walking routine therefore descended into ITSELF one step per
# path element -- `pwd(%toml, <a b c>)` answered `["a", 0, "b", 0, "c"]` where
# rakudo answers `["a", 0, "b", "c"]`, and `Config::TOML` built a nested Array
# where the TOML said Hash (GH #7539).
#
# The trigger is subtle enough to be worth pinning explicitly: an `@`/`%`
# PARAMETER disqualifies the callee from the slot-only light call paths, and
# only the env-chaining paths could see a caller's temp. The identical routine
# with a signature of plain scalars was always correct, which is why the same
# code behaved differently for what looked like a cosmetic signature change.
#
# Every expectation below was measured against rakudo (v2026.07).
use Test;

plan 6;

# --- the minimal shape: recursion + an `@` parameter -------------------------

sub walk($container, @path) {
    return [] unless @path;
    my $root := $container;
    $root := $root{@path[0]};
    [ $root.defined, |walk($root, @path[1..*]) ];
}

my %t = a => { b => 1 };
is-deeply walk(%t, ['a', 'zz', 'yy']), [True, False, False],
   'a rebind to a missing key reads as undefined at every recursion depth';

# The `%` twin, and the shape where the caller is a *different* routine.
sub inner($container, %opts, $k) {
    my $root := $container;
    $root := $root{$k};
    $root.defined;
}
sub outer($container, $k) {
    my $root := $container;
    $root := $root{$k};
    inner($root, {}, 'missing');
}
nok outer(%t, 'a'), 'a rebind in a callee with a % parameter is not the caller container';

# --- the `pwd` descent Config::TOML actually performs ------------------------

multi sub pwd(Associative:D $container, @ ($step, *@rest) --> Array:D) {
    my @step-taken;
    my $root := $container;
    $root := $root{$step};
    push(@step-taken, $step, |pwd($root, @rest));
    @step-taken;
}
multi sub pwd(Associative:D $, @ --> Array:D) { my @step-taken; }
multi sub pwd(Positional:D $container, @step where .elems > 0 --> Array:D) {
    my @step-taken;
    my $root := $container;
    my Int:D $index = $container.end;
    $root := $root[$index];
    push(@step-taken, $index, |pwd($root, @step));
    @step-taken;
}
multi sub pwd(Positional:D $, @ --> Array:D) { my @step-taken; }
multi sub pwd($container, @ ($step, *@rest) --> Array:D) {
    my @step-taken;
    my $root := $container;
    $root := try $root{$step};
    push(@step-taken, $step, |pwd($root, @rest));
    @step-taken;
}
multi sub pwd($, @ --> Array:D) { my @step-taken; }

my %toml;
%toml<a> = [];
%toml<a>.push({});

is-deeply pwd(%toml, ['a', 'b', 'c']), ['a', 0, 'b', 'c'],
   'the arraytable path descent stops at the first missing key';

# --- the bind itself still works where the key DOES exist --------------------

sub reach($container, @path) {
    my $root := $container;
    $root := $root{@path[0]};
    $root := $root{@path[1]};
    $root;
}
is reach(%t, ['a', 'b']), 1, 'a chained rebind through existing keys reaches the leaf';

# A rebind to an existing key still hands back a writable container.
sub bind-leaf($container, @path) is rw {
    my $root := $container;
    $root := $root{@path[0]};
    return-rw $root{@path[1]};
}
my %w = a => { b => 1 };
bind-leaf(%w, ['a', 'b']) = 5;
is %w<a><b>, 5, 'a rebind through an @ parameter still writes through to the source';

# And a rebind to a MISSING key autovivifies the whole path on write.
my %v = a => { };
sub bind-missing($container, @path) is rw {
    my $root := $container;
    $root := $root{@path[0]};
    return-rw $root{@path[1]};
}
bind-missing(%v, ['a', 'new']) = 7;
is %v<a><new>, 7, 'a rebind to a missing key autovivifies on write';

# vim: expandtab shiftwidth=4
