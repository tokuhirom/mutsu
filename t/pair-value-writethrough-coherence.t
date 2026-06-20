use Test;

# Pin: mutating an Array/Hash element *through* a Pair value (`$p.value<k> = v`,
# `$p.value[i] = v`) writes back to the source variable the Pair aliases, and
# must stay coherent WITHOUT relying on reverse-sync (`MUTSU_NO_REVERSE_SYNC=1`).
# The lvalue-method element-assign builtin clones the container and replaces all
# bindings holding the old Arc via `overwrite_{hash,array}_bindings_by_identity`,
# which writes the env entry by name; it now also records those names so the
# call-site `apply_pending_rw_writeback` refreshes the caller's local slot. This
# must hold identically whether or not reverse-sync runs (raku is the oracle).

plan 9;

# Hash value: $p.value<f> = 3 reaches the source hash.
{
    my $h = { :d(1), :e(2) };
    my $p = ($h => $h);
    $p.value<f> = 3;
    is $p.value<f>, 3, 'hash element assigned through pair value';
    is $h<f>, 3, 'pair-value hash write reaches source var (locals coherent)';
    ok $p.value =:= $h, 'pair value and source stay the same container';
}

# Array value: $p.value[3] = "x" reaches the source array.
{
    my @src = <a b c>;
    my $a = @src;
    my $p = (k => $a);
    $p.value[3] = "x";
    is $p.value[3], "x", 'array element assigned through pair value';
    is $a[3], "x", 'pair-value array write reaches source var (locals coherent)';
}

# Repeated writes accumulate coherently through the source var.
{
    my $h = { :a(1) };
    my $p = ($h => $h);
    $p.value<b> = 2;
    $p.value<c> = 3;
    is $h<b>, 2, 'first pair-value write visible via source';
    is $h<c>, 3, 'second pair-value write visible via source';
}

# Source-side write is still visible through the pair value (regression guard).
{
    my $h = { :x(1) };
    my $p = ($h => $h);
    $h<y> = 9;
    is $p.value<y>, 9, 'source-side write reaches pair value';
}

# Array key: push $p.key reaches the source array.
{
    my $a = [< a b c >];
    my $p = ($a => 1);
    push $p.key, "d";
    is ~$p.key, ~$a, 'pushing the pair key reaches the source array';
}
