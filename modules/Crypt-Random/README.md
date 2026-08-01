# Crypt::Random
[![Build Status](https://travis-ci.org/skinkade/crypt-random.svg?branch=master)](https://travis-ci.org/skinkade/crypt-random) [![Build status](https://ci.appveyor.com/api/projects/status/9w39bpjclskckwep/branch/master?svg=true)](https://ci.appveyor.com/project/skinkade/crypt-random)

Random numbers and bytes mimicking `arc4random()`.


## Synopsis
```
use Crypt::Random;

# Random positive Int, defaulting to 32-bit
my Int $foo = crypt_random();
my Int $foo64 = crypt_random(Int(64/8));

# Random Int between 0 and $upper_bound (exclusive)
my Int $bar = crypt_random_uniform($upper_bound);
my Int $bar128 = crypt_random_uniform($upper_bound, Int(128/8));

# Buf of $len random bytes
my Buf $baz = crypt_random_buf($len);
```

## Extra
Additional useful functions built upon the above primitives.
```
use Crypt::Random::Extra;

my Str $uuid = crypt_random_UUIDv4();

my Int $prime = crypt_random_prime()
my Int $prime2048 = crypt_random_prime(Int(2048/8));

# Random sampling of $set, which can be a Blob or List
my Array @sample = crypt_random_sample($set, $count);
```

## Entropy Sources
Random bytes are drawn from `/dev/urandom` on Unix-like systems, and
`CryptGenRandom()` on Windows.

## Copyright & License
Copyright 2016 Shawn Kinkade.

This module may be used under the terms of the Artistic License 2.0.
