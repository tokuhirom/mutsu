use Test;

# #9244: a routine's own `my $r := ...` is a fresh binding. A mainline `$r`
# of the same name that is already bound (to a Proxy, or to another variable)
# must neither receive the new binding nor any later `$r = v` store.

plan 8;

{
    my $t = "ab";
    my $r := substr-rw($t, 0, 1);
    $r = "x";
    sub f { my $s = "hello"; my $r := substr-rw($s, 0, 1); $r = "J"; $s }
    is f(), "Jello", 'own substr-rw Proxy receives the store despite an outer bound Proxy';
    is $t, "xb", 'the outer substr-rw Proxy is left alone';
    $r = "y";
    is $t, "yb", 'the outer Proxy still writes its own source afterwards';
}

{
    my $b = Buf.new(1, 2, 3);
    my $r := subbuf-rw($b, 0, 1);
    sub g { my $s = "hello"; my $r := substr-rw($s, 0, 1); $r = "J"; $s }
    is g(), "Jello", 'own substr-rw Proxy vs. an outer subbuf-rw Proxy';
    is $b.list.join(","), "1,2,3", 'the outer buffer is untouched';
}

{
    my $x = 1;
    my $r := $x;
    sub h { my $s = "hello"; my $r := substr-rw($s, 0, 1); $r = "J"; $s }
    is h(), "Jello", 'own Proxy binding despite an outer variable alias of the same name';
    is $x, 1, 'the outer alias source does not receive the Proxy or the store';
    is $x.VAR.^name, "Scalar", 'the outer alias source keeps its Scalar container';
}
