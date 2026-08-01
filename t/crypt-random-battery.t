use Test;

# Crypt::Random is bundled (BATTERIES.md §7, docs/batteries/crypt-random.md), so
# it must load and work with a plain `use` -- no `-I`, no install. This pins the
# zero-config resolution and a smoke slice of the API; the exhaustive behaviour
# check is the release-time gate that runs the full upstream suite
# (scripts/battery-testsuite.sh).

plan 10;

use Crypt::Random;
use Crypt::Random::Extra;

my $buf = crypt_random_buf(32);
is $buf.elems, 32, 'crypt_random_buf returns the requested number of bytes';
nok $buf eqv crypt_random_buf(32), 'two crypt_random_buf outputs differ';

my $n = crypt_random();
ok $n >= 0, 'crypt_random returns a non-negative Int';
isnt $n, crypt_random(), 'two crypt_random outputs differ';

my $u = crypt_random_uniform(10000);
ok 0 <= $u < 10000, 'crypt_random_uniform stays below the upper bound';
is crypt_random_uniform(1), 0, 'crypt_random_uniform(1) is always 0';

my $uuid = crypt_random_UUIDv4();
is $uuid.chars, 36, 'crypt_random_UUIDv4 has the 8-4-4-4-12 shape';
ok $uuid ~~ /^ <[0..9a..f]> ** 8 '-' <[0..9a..f]> ** 4 '-' 4 <[0..9a..f]> ** 3
              '-' <[89ab]> <[0..9a..f]> ** 3 '-' <[0..9a..f]> ** 12 $/,
    'crypt_random_UUIDv4 sets the version-4 and variant bits';

ok crypt_random_prime(2).is-prime, 'crypt_random_prime returns a prime';

my @sample = crypt_random_sample([1, 2, 3, 4, 5], 3);
is @sample.elems, 3, 'crypt_random_sample returns the requested count';
