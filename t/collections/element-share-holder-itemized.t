use Test;

# ADR-0079 slices 3-4: an `Array`/`Hash` element is a `Scalar` holder, so an
# aggregate assigned into one reads back itemized while the source variable
# does not; and a hash initializer unwraps every element container
# unconditionally, leaving an itemized value opaque and a `List` element's
# hash flattening. Every expectation was measured on rakudo.

plan 14;

my %h = a => 1;
my @r = 1, 2;

{
    my @a; @a[0] = %h; @a[1] = @r;
    is @a[0].raku, '${:a(1)}', 'a hash assigned to an array element reads itemized';
    is @a[1].raku, '$[1, 2]', 'an array assigned to an array element reads itemized';
    is %h.raku, '{:a(1)}', 'the source hash itself stays plain';
    is @a.raku, '[{:a(1)}, [1, 2]]', 'Array.raku elides its elements\' itemization';
    throws-like { my %c = (@a[0],) }, X::Hash::Store::OddNumber,
        'an itemized element stays one opaque hash-initializer item';
    my $r := @a[0];
    is $r.raku, '${:a(1)}', 'a binding to the element sees the itemized holder';
}

{
    my %k; %k<x> = %h; %k<y> = @r;
    is %k<x>.raku, '${:a(1)}', 'a hash assigned to a hash value reads itemized';
    is %k.raku, '{:x(${:a(1)}), :y($[1, 2])}', 'Hash.raku shows itemized values';
    throws-like { my %c = (%k<x>,) }, X::Hash::Store::OddNumber,
        'an itemized hash value stays one opaque hash-initializer item';
}

{
    # The share is still a share: the element and the source are one container.
    my %s = a => 1;
    my @a; @a[0] = %s;
    %s<b> = 2;
    is-deeply @a[0], %s, 'a later write to the source is seen through the element';
    @a[0] = 5;
    is %s.raku, '{:a(1), :b(2)}', 'reassigning the element replaces it, not the source';
}

{
    # A `List`'s elements are not containers: its hash flattens.
    my @l := 1, %h;
    my %c = (@l[1],);
    is-deeply %c, %(a => 1), 'a List element\'s hash flattens into a hash initializer';
    my $l = (1, %h);
    my %d = ($l[1],);
    is-deeply %d, %(a => 1), 'so does one read out of an itemized List';
    my %e = (%h,);
    is-deeply %e, %(a => 1), 'and a bare hash variable';
}
