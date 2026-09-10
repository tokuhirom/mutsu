use Test;

# Pin: a deep `:=` element bind (and a deep element write afterwards) must keep
# the locals store and the env store coherent WITHOUT relying on reverse-sync
# (`MUTSU_NO_REVERSE_SYNC=1`). The deep nested index-assign op descends from the
# env copy of the root scalar (COW-detaching its outer Arc) and previously
# invalidated the locals slot to Nil, trusting reverse-sync to re-pull it. It now
# write-throughs the updated root back into the local slot, so a later read via
# the locals store sees the freshly bound cell / mutated path. This must hold
# identically whether or not reverse-sync is active (raku is the oracle).

plan 13;

# --- RHS bind, deep mixed array/hash path ---
{
    my $struct = [ "ignored", { key => { subkey => [ "ignored", 42 ] } } ];
    my $abbrev := $struct[1]<key><subkey>[1];
    is $abbrev, 42, 'RHS deep bind reads through binding';
    $struct[1]<key><subkey>[1] = 43;
    is $abbrev, 43, 'RHS deep: element write -> binding';
    $abbrev = 44;
    is $struct[1]<key><subkey>[1], 44, 'RHS deep: binding write -> element (locals coherent)';
}

# --- LHS bind, deep mixed array/hash path ---
{
    my $struct = [ "ignored", { key => { subkey => [ "ignored", 42 ] } } ];
    my $abbrev = 30;
    $struct[1]<key><subkey>[1] := $abbrev;
    is $abbrev, 30, 'LHS bind leaves the source value unchanged';
    $struct[1]<key><subkey>[1] = 31;
    is $abbrev, 31, 'LHS deep: element write -> source';
    $abbrev = 32;
    is $struct[1]<key><subkey>[1], 32, 'LHS deep: source write -> element (locals coherent)';
}

# --- LHS bind, deep array-of-array path, sibling untouched ---
{
    my $s = [ 0, [ 0, [ 5, 99 ] ] ];
    my $v = 7;
    $s[1][1][0] := $v;
    is $v, 7, 'LHS deep array bind leaves source unchanged';
    $s[1][1][0] = 8;
    is $v, 8, 'LHS deep array: element write -> source';
    $v = 9;
    is $s[1][1][0], 9, 'LHS deep array: source write -> element (locals coherent)';
    is $s[1][1][1], 99, 'LHS deep array: sibling element untouched';
}

# --- Plain deep write (no bind) stays coherent without reverse-sync ---
{
    my $g = { outer => { inner => 10 } };
    $g<outer><inner> = 55;
    is $g<outer><inner>, 55, 'deep hash write read-back (locals coherent)';
}

# --- Deep write through an intermediate array, then read ---
{
    my $a = [ [ 1, 2 ], [ 3, 4 ] ];
    $a[1][0] = 99;
    is $a[1][0], 99, 'deep array write read-back (locals coherent)';
    is $a[0][1], 2, 'deep array write: sibling untouched';
}
