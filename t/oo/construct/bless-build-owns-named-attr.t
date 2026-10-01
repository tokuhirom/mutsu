use Test;

# From Net::Netmask t/05-construction6.t: `self.bless(:address(...))` hands the
# value to `submethod BUILD(:@address)`; it must not also be stored (and
# element-type-checked) into the typed `has Int @.address` attribute first.
plan 3;

class C {
    has Int @.address;
    submethod BUILD(:@address) { @!address = 1, 2 }
}
my @a = 1, ();
my $c;
lives-ok { $c = C.bless(:address(@a)) }, 'bless with a BUILD-owned typed @ attribute lives';
is $c.address.raku, 'Array[Int].new(1, 2)', 'BUILD sets the attribute';

class D {
    has Int $.x;
    has Int $.y;
    submethod BUILD(:$y) { $!y = $y * 2 }
}
is D.bless(:x(5), :y(3)).y, 6, 'BUILD still receives its named args';
