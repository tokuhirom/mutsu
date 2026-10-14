use Test;

# `Hash.AT-KEY` is `is raw` in rakudo: the method form answers the element's
# own container, so a binding to it, an `is rw` parameter and a `callsame`
# from a user `AT-KEY` all write back into the hash (#13207).

plan 16;

{
    my %h = a => 1;
    my $c := %h.AT-KEY('a');
    is $c.VAR.^name, 'Scalar', 'AT-KEY answers a Scalar container';
    $c = 5;
    is-deeply %h, {a => 5}, 'assigning through the bound container writes the hash';
    is %h.AT-KEY('a'), 5, 'a plain read still answers the value';
    my $x = %h.AT-KEY('a');
    $x = 9;
    is-deeply %h, {a => 5}, 'assignment to a variable copies the value out';
}

{
    my %h;
    my $m := %h.AT-KEY('z');
    is-deeply %h, {}, 'binding a missing key does not create it';
    $m = 3;
    is-deeply %h, {z => 3}, 'the first write vivifies the entry';
    is %h.AT-KEY('q').raku, 'Any', 'a missing key reads as the default';
    my Int %t;
    is %t.AT-KEY('q').raku, 'Int', 'a typed hash reads its value type';
}

{
    my %h = a => 1;
    sub bump($o is rw) { $o = $o + 41 }
    bump(%h.AT-KEY('a'));
    is %h<a>, 42, 'an is rw parameter writes through AT-KEY';
}

{
    my Map $m = Map.new((a => 1));
    is $m.AT-KEY('a').VAR.^name, 'Int', 'a Map element is a value, not a container';
}

{
    my @seen;
    class D is Hash {
        method AT-KEY($k) is rw {
            my $v := callsame;
            @seen.push: $v.VAR.^name;
            $v
        }
    }
    my $d = D.new;
    $d<a> = 1;
    is @seen.elems, 0, 'assigning to an `is Hash` subclass does not call its AT-KEY';
    is $d.AT-KEY('a'), 1, 'the user AT-KEY reads the element';
    is-deeply @seen, ['Scalar'], 'callsame answers the element container';
}

{
    # The PDF::COS::Tie::Hash shape: coerce the entry in place through a
    # chain of `is rw` parameters.
    sub coerce($obj is rw) { $obj = "<$obj>" }
    class T is Hash {
        method AT-KEY($k) is rw {
            my $v := callsame;
            coerce($v);
            $v
        }
    }
    my $t = T.new;
    $t<k> = 'v';
    is $t<k>, '<v>', 'the coerced value is returned';
    is $t.raku.contains('"<v>"'), True, 'and stored back in the hash';
}

{
    my @seen;
    class A is Array {
        method AT-POS($i) is rw { @seen.push: $i; callsame }
    }
    my $a = A.new;
    $a[0] = 1;
    is @seen.elems, 0, 'assigning to an `is Array` subclass does not call its AT-POS';
}
