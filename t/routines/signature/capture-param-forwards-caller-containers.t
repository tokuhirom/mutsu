use Test;

# A `|c` capture binds its arguments raw, so forwarding it (`g(|c)`) hands the
# callee the caller's own containers (#11295).

plan 13;

sub bump($x is rw) { $x++ }
sub fwd(|c) { bump(|c) }
sub fwd2(|c) { fwd(|c) }

{
    my $a = 1;
    fwd($a);
    is $a, 2, 'g(|c) reaches an is rw parameter with the caller variable';
    fwd2($a);
    is $a, 3, 'two levels of |c forwarding keep the container';
}

{
    sub run-in-sub { my $z = 10; fwd($z); fwd($z); $z }
    is run-in-sub(), 12, 'a routine-local variable forwarded through |c';
}

{
    sub raw-target(\x) { x = 'set' }
    sub fwd-raw(|c) { raw-target(|c) }
    my $v = 'orig';
    fwd-raw($v);
    is $v, 'set', 'a sigilless parameter writes through a forwarded capture';
}

{
    sub assign-first(|c) { c[0] = 42 }
    my $d = 1;
    assign-first($d);
    is $d, 42, 'assigning to a capture element writes the caller variable';
}

{
    class Items {
        has $.blob;
        method new(|c) { self.bless!add-items: |c }
        method !add-items(\blob, $offset is rw) { $offset += 5; self }
    }
    my $off = 1;
    Items.new('x', $off);
    is $off, 6, 'method new(|c) forwards a container to a private is rw parameter';
}

{
    sub show(|c) { c }
    my $b = 3;
    my $c = show($b, 4, :k($b));
    is $c.raku, '\\(3, 4, :k(3))', 'a container-holding capture still prints its values';
    is $c.elems, 2, 'elems counts the positionals';
    is $c[0], 3, 'a positional reads as its value';
    ok show(1, 2) eqv \(1, 2), 'eqv against a literal capture';
}

{
    sub fwd-lit(|c) { bump(|c) }
    throws-like { fwd-lit(5) }, X::Parameter::RW,
        'a literal forwarded to an is rw parameter is still rejected';
}

{
    sub first(|c) { c[0] }
    is first(Int).raku, 'Int', 'a type-object argument is passed as a value';
    my \t = 7;
    is first(t), 7, 'a sigilless value binding is passed as a value';
}
