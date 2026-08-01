use v6;
use strict;
use Crypt::Random;

unit module Crypt::Random::Extra;



sub crypt_random_UUIDv4 returns Str is export {
    my $buf = crypt_random_buf(16);
    $buf[6] +|= 0b01000000;
    $buf[6] +&= 0b01001111;
    $buf[8] +|= 0b10000000;
    $buf[8] +&= 0b10111111;

    # skids is a wizard
    (:256[$buf.values].fmt("%32.32x")
        ~~ /(........)(....)(....)(....)(............)/)
        .join("-");
}

subset PosUInt32 of Int where 1 .. 2**32 - 1;
sub crypt_random_prime(PosUInt32 $size = 4) returns Int is export {
    my $prime = Int.new;

    loop (;;) {
        $prime = crypt_random($size);
        if ($prime.is-prime) {
            return $prime;
        }
    }
}

sub crypt_random_sample($set where List|Blob,
                        Int $count) returns Array is export {

    my @sample;

    for ^$count {
        @sample.push($set[crypt_random_uniform($set.elems)]);
    }

    @sample;
}
