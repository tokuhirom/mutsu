use v6;
use Test;

# A typed `is raw` / `is rw` parameter handed a bare Proxy (the result of a
# container-returning routine, no caller variable to alias) binds the Proxy
# itself, so assigning to the parameter fires its STORE. Found by
# RedX::HashedPassword: `sub deflate(Str $password is raw) { $password = ... }`
# called as `$column.deflate.($attr.get_value($model))`. (The direct named-sub
# call `deflate($attr.get_value($m))` FETCHes at the call site first; that is
# tracked separately.)

plan 4;

class C { has Str $.t is rw; }
my $c = C.new;
my $store = "plain";
my \proxy = Proxy.new(
    FETCH => method { $store },
    STORE => method (\value) { $store = value },
);
my $attr = C.^attributes[0];
$attr.set_value($c, proxy);

sub typed-raw(Str $s is raw) { $s = "typed-raw" }
sub untyped-raw($s is raw) { $s = "untyped-raw" }
sub typed-rw(Str $s is rw) { $s = "typed-rw" }

&typed-raw.($attr.get_value($c));
is $store, 'typed-raw', 'typed `is raw` parameter assigns through the Proxy';
&untyped-raw.($attr.get_value($c));
is $store, 'untyped-raw', 'untyped `is raw` parameter assigns through the Proxy';
&typed-rw.($attr.get_value($c));
is $store, 'typed-rw', 'typed `is rw` parameter assigns through the Proxy';

sub reads(Str $s is raw) { $s }
is &reads.($attr.get_value($c)), 'typed-rw', 'a typed raw parameter still type-checks the FETCHed value';
