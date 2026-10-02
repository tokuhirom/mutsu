use Test;

# #10897: an element write through an attribute accessor lands in the
# attribute's own container, so a `:=` alias bound to it earlier stays the
# same container.

plan 10;

{
    class H { has %.u is rw }
    my $a = H.new;
    my $t := $a.u;
    $a.u<o> = 7;
    $t<o> = 1;
    is-deeply $a.u, %(o => 1), 'hash attribute: alias write after an accessor write';
    is $a.u.WHICH, $t.WHICH, 'the alias and the attribute are one container';
}

{
    class HK { has %.u{Str:D} is rw }
    my $a = HK.new;
    my $t := $a.u;
    $a.u<o> = 7;
    $t<o> = 1;
    is $a.u<o>, 1, 'object-hash attribute: the alias write is seen';
    is $a.u.keys.map({ .^name }).join(','), 'Str', 'keyed by the key object';
}

{
    class A { has @.u is rw }
    my $a = A.new;
    my $t := $a.u;
    $a.u[1] = 7;
    $t[0] = 1;
    is-deeply $a.u.List, (1, 7), 'array attribute: both writes land in one container';
    is-deeply $t.List, (1, 7), 'and the alias sees them';
}

{
    class C { has %.u is rw }
    my %src = a => 1;
    my $c = C.new(u => %src);
    $c.u<z> = 1;
    is-deeply %src, %(a => 1), 'a hash the attribute was built from is not written';
    my %copy = $c.u;
    $c.u<y> = 2;
    nok %copy<y>:exists, 'nor is a copy of the attribute';
    my $d = $c.clone;
    $c.u<x> = 3;
    is $d.u<x>, 3, 'a clone shares the attribute container, as in rakudo';
    is $c.u<x>, 3, 'and the original holds the write';
}
