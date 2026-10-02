use Test;

# `$s = $src` marks the store as a possible array share (`$s = @a` shares the
# container by reference). The mark is now left unset when the value being
# stored cannot be an Array/Hash (#10955), so each shape here pins that the
# share still happens for aggregates and a plain copy still copies.

plan 14;

{
    my $i = 1;
    my $s = 0;
    $s = $i;
    $i = 2;
    is $s, 1, 'Int source is copied, not aliased';
}

{
    my @a = 1, 2;
    my $s;
    $s = @a;
    @a.push: 3;
    is $s.elems, 3, '$s = @a still shares the array';
}

{
    my %h = a => 1;
    my $s;
    $s = %h;
    %h<b> = 2;
    is $s.elems, 2, '$s = %h still shares the hash';
}

{
    my @a = 1, 2;
    my $q = @a;
    my $r;
    $r = $q;
    $q.push: 3;
    is $r.elems, 3, 'chained $r = $q shares an array held by $q';
}

{
    my $q = 5;
    my $r;
    $r = $q;
    $q = [1, 2];
    is $r, 5, 'chained $r = $q copies an Int';
    $r = $q;
    $q.push: 3;
    is $r.elems, 3, 'the same site shares once $q holds an array';
}

{
    my $str = "abc";
    my $rat = 1/3;
    my $pair = a => 1;
    my $range = 1..3;
    my $type = Int;
    my ($s1, $s2, $s3, $s4, $s5);
    $s1 = $str; $s2 = $rat; $s3 = $pair; $s4 = $range; $s5 = $type;
    $str = "x"; $rat = 0; $pair = b => 2; $range = 5..6; $type = Str;
    is-deeply ($s1, $s2, $s3, $s4, $s5), ("abc", 1/3, a => 1, 1..3, Int),
        'scalar-kind sources are copied';
}

{
    my $i = 7;
    my $s = 0;
    is ($s = $i), 7, 'assignment as an expression yields the value';
    $i = 8;
    is $s, 7, 'and copied it';
}

{
    my @a = 1, 2;
    my $s;
    my $t = ($s = @a);
    @a.push: 3;
    is $s.elems, 3, 'assignment-expression form still shares the array';
}

{
    my $x = 0;
    for ^5 -> $i { my $s = 0; $s = $i; $x += $s }
    is $x, 10, 'loop of plain stores';
}

{
    class C {
        has $.v = 3;
        method m() { my $s = 0; my $i = $!v; $s = $i; $s }
    }
    is C.new.m, 3, 'plain store inside a method';
}

{
    my @a = 1, 2;
    my $p := Proxy.new(FETCH => -> $ { @a }, STORE => -> $, $ { });
    my $s;
    $s = $p;
    @a.push: 3;
    is $s.elems, 3, 'a Proxy fetching an array is still shared';
}

{
    my Int $i = 4;
    my Int $s = 0;
    $s = $i;
    is $s, 4, 'typed scalar store';
}
