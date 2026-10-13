use Test;

plan 7;

{
    my $h = {};
    my $t := $h<a>;
    $t.push: {};
    is $t.WHAT.raku, 'Array', 'bound variable reads as the autovivified Array';
    is $t.raku, '$[{},]', '.raku of the bound variable';
    is $t.elems, 1, '.elems';
}

{
    my $h = {};
    my $t := $h<a>;
    $t.push: {};
    $t := $t[0];
    $t<x> = 5;
    is $h.raku, '${:a($[{:x(5)},])}', 'rebinding through the variable reaches the hash';
}

{
    my %h;
    my $t := %h<k>;
    $t.push: 1;
    $t.push: 2;
    is-deeply %h<k>, [1, 2], 'second push appends to the same Array';
    is $t.elems, 2, 'variable sees both pushes';
}

{
    my $h = {};
    my $t := $h<a>;
    is $t.WHAT.raku, 'Any', 'an unpushed bound missing entry still reads as Any';
}
