use Test;

# (B) per-store env-write gate (MUTSU_GATE_LOCAL_ENV_WRITE) regression pin.
#
# A scalar local holding a container (`my $b = "hi".encode`, `my $m = %h.Map`)
# skips its env mirror under the gate, so an indexed assignment
# (`exec_index_assign_expr_named_op_inner`) that reads the container from env
# would see the stale `my`-decl seed (Any) instead of the live Blob/Map. On an
# immutable container that made the assignment silently NOT throw. The handler
# now seeds env from the authoritative slot at entry (gated; OFF byte-identical).
# This pin passes OFF, ON (MUTSU_GATE_LOCAL_ENV_WRITE=1), and under real raku.

plan 8;

# An encode result is an immutable Blob — element assignment throws.
{
    my $died = False;
    { my $b = "hi".encode; $b[0] = 200; CATCH { default { $died = True } } }
    ok $died, 'indexed assign on a scalar-held Blob throws (immutable)';
}

# A Map is immutable — key assignment throws.
{
    my %h = a => 1, b => 2;
    my $m = %h.Map;
    my $died = False;
    { $m<c> = 9; CATCH { default { $died = True } } }
    ok $died, 'indexed assign on a scalar-held Map throws (immutable)';
}

# A scalar-held mutable Array still assigns in place and is visible.
{
    my $a = [1, 2, 3];
    $a[0] = 99;
    is $a[0], 99, 'scalar-held Array element assign is visible';
    is $a.elems, 3, 'scalar-held Array keeps its shape';
}

# A scalar-held mutable Hash still assigns in place and is visible.
{
    my $hh = { x => 1 };
    $hh<y> = 2;
    is $hh<x>, 1, 'scalar-held Hash keeps the old key';
    is $hh<y>, 2, 'scalar-held Hash element assign is visible';
}

# Plain aggregate lexicals are unaffected (shared-Arc identity).
{
    my @arr = 10, 20, 30;
    @arr[1] = 200;
    is @arr[1], 200, 'plain @-array element assign is visible';
}
{
    my %hsh = a => 1;
    %hsh<b> = 2;
    is %hsh.elems, 2, 'plain %-hash element assign is visible';
}
