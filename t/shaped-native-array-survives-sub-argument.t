use Test;

plan 24;

# A shaped (fixed-dimension) array must keep its shape after the variable has
# been shared into a scalar container -- by a `$scalar = @arr` share, by an
# argument capture, or by a `:=` rebind used as an argument. Those all replace
# the variable's slot with a shared `ContainerRef` cell, and the whole-array
# assignment path used to read the cell instead of the array inside it, so the
# shape was lost and the next `@arr = ...` silently shrank the array.

sub peek(Mu $got) { $got.defined }
sub peek2($got)   { $got.defined }
sub peek3(@got)   { @got.elems }

# --- native shaped array, via a sub argument -------------------------------
{
    my @a := array[str].new(:shape(4), "a", "b", "c", "d");
    peek(@a);
    @a = "x", "y";
    is @a.elems, 4, 'native shaped str array keeps its shape across a sub argument';
    is @a.join(':'), 'x:y::', 'and refills the trailing slots with the element default';
    is-deeply @a.shape.List, (4,), '.shape still reports the fixed dimension';
}

# --- the assignment's own result as the argument ---------------------------
{
    my @b := array[str].new(:shape(4), "a", "b", "c", "d");
    peek((@b = ()));
    @b = "x", "y";
    is @b.elems, 4, 'the result of `@arr = ()` as a sub argument does not unshape @arr';
    is @b.join(':'), 'x:y::', 'and the later assignment still refills';
}

# --- plain scalar copy / bind ----------------------------------------------
{
    my @c[4] = "a", "b", "c", "d";
    my $copy = @c;
    @c = "x", "y";
    is @c.elems, 4, '`my $s = @shaped` does not unshape @shaped';
    is $copy.elems, 4, 'and the shared scalar sees the refilled array';
}

{
    my @d[4] = "a", "b", "c", "d";
    my $alias := @d;
    @d = "x", "y";
    is @d.elems, 4, '`my $s := @shaped` does not unshape @shaped';
    is $alias.elems, 4, 'and the bound scalar sees the refilled array';
}

# --- every parameter shape -------------------------------------------------
{
    my @e[4] = "a", "b", "c", "d";
    peek2(@e);
    @e = "x", "y";
    is @e.elems, 4, 'an untyped scalar parameter does not unshape the argument';
}

{
    my @f[4] = "a", "b", "c", "d";
    peek3(@f);
    @f = "x", "y";
    is @f.elems, 4, 'an `@` parameter does not unshape the argument';
}

# --- native int/num keep their element defaults ----------------------------
{
    my @g := array[int].new(:shape(3), 1, 2, 3);
    peek(@g);
    @g = 7, 8;
    is @g.elems, 3, 'native shaped int array keeps its shape';
    is-deeply @g.List, (7, 8, 0), 'and pads with the int element default';
}

{
    my @h := array[num].new(:shape(3), 1e0, 2e0, 3e0);
    peek(@h);
    @h = 7e0, 8e0;
    is @h.elems, 3, 'native shaped num array keeps its shape';
    is-deeply @h.List, (7e0, 8e0, 0e0), 'and pads with the num element default';
}

# --- the element type survives too -----------------------------------------
{
    my @i := array[str].new(:shape(4), "a", "b", "c", "d");
    peek(@i);
    @i = "x", "y";
    ok @i.of === str, 'the element type survives the reshaping assignment';
}

# --- `.squish` reads through the shared cell -------------------------------
{
    my @j := array[str].new("m", "e", "a", "t");
    peek((@j := array[str].new("nn", "nn", "bb", "uu")));
    is-deeply @j.squish.List, ("nn", "bb", "uu"),
      '.squish sees the array through the shared cell, not the cell itself';
    is-deeply @j.unique.List, ("nn", "bb", "uu"), '.unique agrees';
    is-deeply @j.repeated.List, ("nn",), '.repeated agrees';
    is @j.elems, 4, 'and the array itself is unchanged';
}

{
    my @k := array[int].new(11, 11, 13, 17);
    my $share = @k;
    is-deeply @k.squish.List, (11, 13, 17),
      '.squish after a `$scalar = @arr` share';
    is-deeply $share.squish.List, (11, 13, 17),
      'and through the sharing scalar itself';
}

# --- an unshaped array must NOT gain a shape -------------------------------
{
    my @u = "a", "b", "c", "d";
    peek(@u);
    @u = "x", "y";
    is @u.elems, 2, 'a plain array still shrinks on reassignment';
    ok @u.shape[0] ~~ Whatever, 'and reports no fixed dimension';
}
