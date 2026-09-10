use v6;
use Test;

# Regression: `my $ex := $_;` inside a nested `sub` call used to promote the
# TOPIC (`$_`) itself to a shared `ContainerRef` cell and splice that cell
# into every ancestor call frame currently on the stack
# (`vm_var_assign_set_local.rs`'s `source_in_outer_frame` gate treated "some
# ancestor frame can reach `_` via its parent chain" — true for essentially
# every frame, since every routine writes a fresh `$_` into its own env on
# entry — as "the source lives in a genuine outer lexical" worth splicing a
# shared cell into). That corrupted the CALLER's own topic with whatever
# `$_` held inside the nested call, permanently, for the rest of the
# program. Found investigating `roast/S03-operators/range.t` under
# `MUTSU_REAL_TEST=1` — the vendored `Test.rakumod`'s own `throws-like` does
# exactly this `my $ex := $_;` inside its `CATCH { default { ... } }`.

plan 1;

sub inner() {
    my $ex := $_;
    # `$ex` must actually be read afterward: an unused bind takes a
    # different (unaffected) fast dispatch path for this sub.
    $ex.defined;
}

my @seen;
for 'a', 'b', 'c' {
    inner();
    @seen.push($_);
}
is @seen.join(','), 'a,b,c',
    q<a for-loop's topic survives a := $_ bind inside a nested sub call>;
