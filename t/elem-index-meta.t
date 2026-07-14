use v6;
use Test;

# Per-element index metadata (`:=` element binds, `=` container shares into an
# element, and `:delete`d indices) is probed on every element write. Those probes
# are gated on a monotonic "any such key was ever created" latch; these tests pin
# the semantics that must survive the gate.

plan 12;

# --- :delete / :exists, and reassignment clearing the deleted marker ---
{
    my @a = 1, 2, 3;
    @a[1]:delete;
    nok @a[1]:exists, 'deleted index reports not-exists';
    ok  @a[0]:exists, 'a sibling index still exists';
    @a[1] = 9;
    ok  @a[1]:exists, 'reassigning a deleted index clears the deleted marker';
    is  @a[1], 9, 'the reassigned value is stored';
}

# --- element `:=` bind writes through in both directions ---
{
    my $x = 1;
    my @a = 0, 0;
    @a[0] := $x;
    $x = 7;
    is @a[0], 7, 'element bind sees a write to the bound scalar';
    @a[0] = 5;
    is $x, 5, 'writing the bound element writes through to the scalar';
}

# --- element `=` container share: shares, but reassignment REPLACES ---
{
    my @row = 1, 2;
    my @aoa;
    @aoa[0] = @row;
    @row.push(3);
    is @aoa[0].elems, 3, 'element = share sees a push to the shared container';
    @aoa[0] = 42;
    is @aoa[0], 42, 'reassigning a shared element replaces it';
    is @row.elems, 3, 'the source container is untouched by the replace';
}

# --- hash :delete / :exists ---
{
    my %h = a => 1, b => 2;
    %h<a>:delete;
    nok %h<a>:exists, 'deleted hash key reports not-exists';
    ok  %h<b>:exists, 'a sibling hash key still exists';
}

# --- a plain element-write program (the latch-off path) still stores ---
{
    my @p;
    @p[$_] = $_ * 2 for ^5;
    is @p.join(","), "0,2,4,6,8", 'plain element writes work with no element metadata';
}
