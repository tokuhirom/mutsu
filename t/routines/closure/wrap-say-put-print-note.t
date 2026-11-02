use Test;

# From the Dawa ecosystem distribution: its tests capture program output with
# `&put.wrap: { @out.push($_) }`. `say`/`put`/`print`/`note` compile to their own
# opcodes, which never looked at the wrap table, so the wrapper never ran.

plan 8;

{
    my @out;
    my $h = &put.wrap: { @out.push($_) };
    put "hello";
    sub from-sub { put "from sub" }
    from-sub();
    class C { method m { put "from method" } }
    C.new.m;
    is-deeply @out, ["hello", "from sub", "from method"], '&put.wrap catches put from every call site';
    $h.restore;
}

{
    my @out;
    my $h = &say.wrap: { @out.push("say:$_"); callsame };
    say "a";
    is-deeply @out, ["say:a"], '&say.wrap runs, and callsame reaches the real say';
    $h.restore;
}

{
    my @out;
    my $h = &print.wrap: { @out.push("print:$_") };
    print "p";
    is-deeply @out, ["print:p"], '&print.wrap';
    $h.restore;
}

{
    my $h = &uc.wrap: { "WRAPPED" };
    is uc("x"), 'WRAPPED', 'an ordinary builtin keeps working (control)';
    $h.restore;
    is uc("x"), 'X', 'and restores';
}

{
    my @out;
    my $h = &put.wrap: { @out.push($_) };
    $h.restore;
    put "after restore";
    is-deeply @out, [], 'a restored wrapper no longer runs';
}

{
    my @out;
    my $h = &put.wrap: { @out.push($_.uc) };
    put "x" for 1..2;
    is-deeply @out, ["X", "X"], 'the wrapper runs for each call in a loop';
    $h.restore;
}

{
    my @out;
    my $h = &put.wrap: -> |c { @out.push(c.list.join('+')) };
    put 1, 2, 3;
    is-deeply @out, ['1+2+3'], 'the wrapper sees all arguments';
    $h.restore;
}
