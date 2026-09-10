use Test;

# `.VAR.^name` reflects the container a variable lives in. A scalar `:=`-bound
# to a container (`my $r := @a` / `:= %h` / `:= (1,2,3)`) aliases that container
# directly and so has no Scalar container of its own — `.VAR.^name` must report
# the underlying container type, not "Scalar". An assigned scalar (`my $s = ...`)
# IS a Scalar container.

plan 16;

# --- Assigned scalars are Scalar containers ---
{
    my $s = 5;
    is $s.VAR.^name, 'Scalar', 'assigned Int scalar .VAR is Scalar';
    my $a = (1, 2, 3);
    is $a.VAR.^name, 'Scalar', 'assigned List scalar .VAR is Scalar';
}

# --- @ / % variables report their container type (unchanged) ---
{
    my @a = 1, 2, 3;
    is @a.VAR.^name, 'Array', '@-variable .VAR is Array';
    my %h = a => 1;
    is %h.VAR.^name, 'Hash', '%-variable .VAR is Hash';
}

# --- A scalar bound to a List literal reflects List ---
{
    my $l := (1, 2, 3);
    is $l.VAR.^name, 'List', 'scalar bound to a List literal .VAR is List';
    is (1, 2, 3).VAR.^name, 'List', 'List literal .VAR is List';
}

# --- A scalar bound to a whole @ / % variable reflects Array / Hash ---
{
    my @a = 1, 2, 3;
    my $b := @a;
    is $b.VAR.^name, 'Array', 'scalar bound to @a .VAR is Array';
    my %h = a => 1;
    my $hr := %h;
    is $hr.VAR.^name, 'Hash', 'scalar bound to %h .VAR is Hash';
}

# --- Bound scalars still flatten on @-assignment (ItemizeVar semantics) ---
{
    my @a = 1, 2, 3;
    my $b := @a;
    my @c = $b;
    is @c.elems, 3, 'scalar bound to an array flattens on @-assignment';
    my $l := (4, 5, 6);
    my @d = $l;
    is @d.elems, 3, 'scalar bound to a List flattens on @-assignment';
    # An assigned scalar itemizes (does NOT flatten).
    my $s = (7, 8, 9);
    my @e = $s;
    is @e.elems, 1, 'assigned List scalar itemizes on @-assignment';
}

# --- .VAR.name still returns the variable name ---
{
    my $x = 5;
    is $x.VAR.name, '$x', '.VAR.name returns the sigil-qualified variable name';
}

# --- Set / Bag bound scalars reflect their type ---
{
    my $s := <a b c>.Set;
    is $s.VAR.^name, 'Set', 'scalar bound to a Set .VAR is Set';
    my $bag := <a a b>.Bag;
    is $bag.VAR.^name, 'Bag', 'scalar bound to a Bag .VAR is Bag';
}

# --- A re-bound scalar updates its reflected container ---
{
    my @a = 1, 2;
    my $r := @a;
    is $r.VAR.^name, 'Array', 'bound scalar reflects Array before rebind';
    $r := (1, 2, 3);
    is $r.VAR.^name, 'List', 'rebound scalar reflects List after rebind';
}
