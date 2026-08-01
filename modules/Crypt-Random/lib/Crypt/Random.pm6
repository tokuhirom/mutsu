use v6;
use if;
use strict;

use Crypt::Random::Win:if($*DISTRO.is-win);
use Crypt::Random::Nix:if(!$*DISTRO.is-win);

unit module Crypt::Random;



# Shim for function exported by OS-specific module
sub crypt_random_buf(uint32 $len) returns Buf is export {
    _crypt_random_bytes($len);
}

# https://rt.perl.org//Public/Bug/Display.html?id=127813
subset PosUInt32 of Int where 1 .. 2**32 - 1;

# Int from byte array (big endian)
sub crypt_random(PosUInt32 $size = 4) returns Int is export {
    my Int $count = 0;
    ($count +<= 8) += $_ for crypt_random_buf($size).values;
    $count;
}

# Translation of arc4random_uniform() for Perl6 and big Ints
sub crypt_random_uniform(Int $upper_bound, PosUInt32 $size = 4) returns Int is export {
    if ($upper_bound < 2) {
        return 0;
    }

    my $min = (2**($size*8) - $upper_bound) % $upper_bound;
    my $r;

    loop (;;) {
        $r = crypt_random($size);
        if ($r >= $min) {
            last;
        }
    }

    $r % $upper_bound;
}
