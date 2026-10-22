use Test;

plan 6;

# An element store through a Proxy whose FETCH answers a type object (an
# unset array/hash attribute behind an `is entry`-style accessor)
# autovivifies the aggregate and STOREs it through the Proxy.
class Holder {
    has $!arr;
    has $!hash;
    method arr is rw {
        my $val;
        Proxy.new:
            FETCH => sub ($) { $val //= $!arr // Array },
            STORE => sub ($, \v) { $!arr = v; $val = v };
    }
    method hash is rw {
        my $val;
        Proxy.new:
            FETCH => sub ($) { $val //= $!hash // Hash },
            STORE => sub ($, \v) { $!hash = v; $val = v };
    }
    method stored-arr { $!arr }
    method stored-hash { $!hash }
}

my $h = Holder.new;
$h.arr[0] //= 7;
is $h.stored-arr.raku, '$[7]', '//= on an unset array element stores the array';
$h.arr[1] ||= 8;
is $h.stored-arr.raku, '$[7, 8]', '||= extends it';
$h.arr[0] //= 99;
is $h.stored-arr.raku, '$[7, 8]', '//= keeps a defined element';

my $g = Holder.new;
$g.hash<a> = 1;
is $g.stored-hash.raku, '${:a(1)}', 'a hash key store autovivifies the hash';
$g.hash<b> //= 2;
is $g.stored-hash<b>, 2, '//= on a hash key';

my $k = Holder.new;
$k.arr[2] = 5;
is $k.stored-arr.elems, 3, 'an assignment past the end';
