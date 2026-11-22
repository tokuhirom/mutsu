use Test;

# `=:=` of two named variables compares the containers the names resolve to
# here, not the first same-named slot of the routine (#18742: an inner `my $z`
# shadowing an outer one compared against the outer).

plan 2;

{
    my $z = 1;
    sub bind-dyn($f is rw) { $*k := $f }
    {
        my $*k = 16;
        my $z = 17;
        bind-dyn($z);
        ok $*k =:= $z, 'a dynamic bound to the inner $z is that $z';
    }
}

{
    my $a = 1;
    my $b := $a;
    {
        my $a = 2;
        my $c := $a;
        ok $c =:= $a, 'a bind to the inner $a is the inner $a';
    }
}
