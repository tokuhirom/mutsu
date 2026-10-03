use Test;

# A sigilless name (`sub t(\p)`, `my \g := $f`) IS the container it was bound
# to, so a Nil assigned through it decays against THAT container: its
# `is default`, its type object, or Any -- what a direct `$f = Nil` stores
# (#11110; the cell-based aliases are pinned by
# nil-through-alias-restores-container-default.t).

plan 14;

{
    my $q = 1;
    sub t(\p) { p = Nil }
    t($q);
    is $q.raku, 'Any', 'untyped variable through a sigilless parameter';
}

{
    my $d is default(15) = 1;
    sub u(\p) { p = Nil }
    u($d);
    is $d, 15, 'defaulted variable through a sigilless parameter';
}

{
    my Int $i = 1;
    sub v(\p) { p = Nil }
    v($i);
    is $i.raku, 'Int', 'typed variable through a sigilless parameter';
}

{
    my Str $s = 'a';
    sub w(\p) { p = Nil }
    w($s);
    is $s.raku, 'Str', 'another typed variable';
}

{
    my $e is default(4) = 1;
    my $t;
    sub x(\p) { $t = (p = Nil) }
    x($e);
    is $t, 4, 'expression-position assignment yields the default';
    is $e, 4, 'expression-position assignment stores the default';
}

{
    my $f = 1;
    my \g := $f;
    g = Nil;
    is $f.raku, 'Any', 'untyped variable through a sigilless binding';
}

{
    my $h is default(5) = 1;
    my \k := $h;
    k = Nil;
    is $h, 5, 'defaulted variable through a sigilless binding';
}

{
    my $q is default(8) = 1;
    sub c(\r) { r = Nil }
    sub b(\p) { c(p) }
    b($q);
    is $q, 8, 'through a chain of sigilless parameters';
}

{
    my $s = 1;
    sub d(\p) { p = Nil; p.raku }
    is d($s), 'Any', 'the sigilless name reads the decayed value';
    is $s.raku, 'Any', '... and so does the variable';
}

{
    my $u is default(9) = 1;
    sub f(\p) { my &c = { p = Nil }; c() }
    f($u);
    is $u, 9, 'through a closure capturing a sigilless parameter';
}

{
    sub a(\p) { p.raku }
    is a(Nil), 'Nil', 'a sigilless parameter bound to Nil stays Nil';
    my \n = Nil;
    is n.raku, 'Nil', 'a sigilless declaration of Nil stays Nil';
}
