use Test;

# `.push`/`.append`/`.unshift`/`.prepend` on an unset scalar auto-vivifies it
# to an Array held in the scalar's container (`$[...]`). For a package-qualified
# name (`$GLOBAL::n`) the vivified array is a package variable and must outlive
# the routine frame that created it (#10620). Expected values are rakudo's.

plan 12;

{
    sub f { $GLOBAL::vp1.push(1) }
    f();
    is $GLOBAL::vp1.raku, '$[1]', 'a push from a routine survives the frame';
}

{
    sub g { $GLOBAL::vp2.push(1); $GLOBAL::vp2.push(2) }
    g();
    is $GLOBAL::vp2.raku, '$[1, 2]', 'the second push lands in the vivified array';
    g();
    is $GLOBAL::vp2.raku, '$[1, 2, 1, 2]', 'a later call keeps pushing onto it';
}

{
    sub h { $GLOBAL::vp3.append(1, 2) }
    sub u { $GLOBAL::vp4.unshift(1) }
    sub p { $GLOBAL::vp5.prepend(3, 4) }
    h(); u(); p();
    is $GLOBAL::vp3.raku, '$[1, 2]', 'append';
    is $GLOBAL::vp4.raku, '$[1]', 'unshift';
    is $GLOBAL::vp5.raku, '$[3, 4]', 'prepend';
}

{
    role R { $GLOBAL::vp6.push(1) }
    sub f2 { my class C does R { } }
    f2();
    is $GLOBAL::vp6.raku, '$[1]', 'a role body composed from a routine';
}

{
    $GLOBAL::vp7.push(1);
    is $GLOBAL::vp7.raku, '$[1]', 'at the top level the array is itemized too';
}

# The vivified array sits in the scalar's container for any `$` variable.
{
    my $x;
    $x.push(1);
    is $x.raku, '$[1]', 'a lexical scalar holds the array in its container';
    is (gather .take for $x).elems, 1, '...so iterating it sees one item';
    our $o;
    sub q { $o.push(3) }
    q();
    is $o.raku, '$[3]', 'an `our` scalar pushed from a routine';
    my @a;
    @a[0].push(3);
    is @a.raku, '[[3],]', 'an array element is not a `$` variable';
}
