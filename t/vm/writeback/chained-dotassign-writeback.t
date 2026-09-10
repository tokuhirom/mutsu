use Test;

# `$var andthen/orelse/notandthen .=method` mutates the topic, which Raku aliases
# to the chain's root variable, so the `.=` writes back to that variable. This
# holds across nested chains (`$x orelse .=new andthen .=new` parses as
# `$x orelse (… andthen …)`, yet both `.=` still target `$x`).

plan 5;

{
    my Int $x1;
    $x1 notandthen .=new;
    is-deeply $x1, 0, 'notandthen .=new writes back (Int.new == 0)';
}

{
    my Int $x3;
    $x3 orelse .=new andthen .=new: 43;
    is-deeply $x3, 43, 'orelse/andthen chain .=new writes back through the root';
}

{
    my $s = 'foo';
    $s andthen .=uc;
    is-deeply $s, 'FOO', 'andthen .=uc writes back to a defined scalar';
}

# Non-`.=` chain RHS still reads the topic (no spurious retarget).
{
    my $seen;
    42 andthen $seen = $_;
    is-deeply $seen, 42, 'andthen topic $_ still readable in a non-.= RHS';
}

# A plain `orelse` default value is unaffected.
{
    my $u;
    my $r = $u // 99;
    is-deeply $r, 99, 'defined-or default still works';
}
