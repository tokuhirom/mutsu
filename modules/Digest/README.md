![Sparky](https://sparky.sparrowhub.io/badge/grondilu-libdigest-raku?foo=bar)
# Digests in raku

This is a pure [raku](https://raku.org/) repository implementing some digest
algorithms.

As of today (march 2023), raku still is fairly slow so if you need a faster way
to compute digest, consider using a
[nativecall](https://docs.raku.org/language/nativecall) binding to the OpenSSL
library instead.

## Synopsis

Nb.  Since commit 911c292688ad056a98285f7930297c5e1aea3bfb,
there is no `Digest` module anymore, the submodules, `Digest::MD5`, `Digest::SHA1` and
so on must be used directly.

```raku
use Digest::MD5;
say md5      "hello";
use HMAC;
say hmac
  key => "key",
  msg => "The quick brown fox jumps over the lazy dog", 
  hash => &md5,
  block-size => 64;

use Digest::SHA1;
say sha1     "Hola";

use Digest::SHA2;
say sha256   "Привет"; 

use Digest::RIPEMD;
say rmd160   "Saluton";

use Digest::SHA3;
say sha3_256 "Bonjour";
say shake256 "Merhaba", 16;

# This will keep printing blocks
.say for shake256 "नमस्ते", *;
```
    
## Features

Currently implemented:

* HMAC
  - hmac
* Digest
  - md5
* Digest::SHA1
  - sha1
* Digest::SHA2
  - sha224
  - sha256
  - sha384
  - sha512
* Digest::SHA3
  - sha3\_224
  - sha3\_256
  - sha3\_384
  - sha3\_512
  - shake128
  - shake256
* Digest::RIPEMD :
  - rmd160

## License

This work is published under the terms of the artistic license, as rakudo is.
See LICENSE file.

