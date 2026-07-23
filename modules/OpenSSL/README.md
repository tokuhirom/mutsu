[![Actions Status](https://github.com/raku-community-modules/OpenSSL/actions/workflows/linux.yml/badge.svg)](https://github.com/raku-community-modules/OpenSSL/actions) [![Actions Status](https://github.com/raku-community-modules/OpenSSL/actions/workflows/macos.yml/badge.svg)](https://github.com/raku-community-modules/OpenSSL/actions) [![Actions Status](https://github.com/raku-community-modules/OpenSSL/actions/workflows/windows.yml/badge.svg)](https://github.com/raku-community-modules/OpenSSL/actions)

NAME
====

OpenSSL - OpenSSL bindings

SYNOPSIS
========

```raku
use OpenSSL;
my $openssl = OpenSSL.new;
$openssl.set-fd(123);
$openssl.write("GET / HTTP/1.1\r\nHost: somehost\r\n\r\n");
```

DESCRIPTION
===========

A module which provides OpenSSL bindings, making us able to set up a TLS/SSL connection.

METHODS
=======

method new
----------

```raku
method new(Bool :$client = False, Int :$version?)
```

A constructor. Initializes OpenSSL library, sets method and context. If $version is not specified, the highest possible version is negotiated.

method set-fd
-------------

```raku
method set-fd(OpenSSL:, int32 $fd)
```

Assigns connection's file descriptor (file handle) $fd to the SSL object.

To get the $fd we should use C to set up the connection. (See [NativeCall](NativeCall)) I hope we will be able to use Raku's IO::Socket module instead of connecting through C soon-ish.

method set-connect-state
------------------------

```raku
method set-connect-state(OpenSSL:)
```

Sets SSL object to connect (client) state.

Use it when you want to connect to SSL servers.

method set-accept-state
-----------------------

```raku
method set-accept-state(OpenSSL:)
```

Sets SSL object to accept (server) state.

Use it when you want to provide an SSL server.

method connect
--------------

```raku
method connect(OpenSSL:)
```

Connects to the server using $fd (passed using .set-fd).

Does all the SSL stuff like handshaking.

method accept
-------------

```raku
method accept(OpenSSL:)
```

Accepts new client connection.

Does all the SSL stuff like handshaking.

method write
------------

```raku
method write(OpenSSL:, Str $s)
```

Sends $s to the other side (server/client).

method read
-----------

```raku
method read(OpenSSL:, Int $n, Bool :$bin)
```

Reads $n bytes from the other side (server/client).

Bool :$bin if we want it to return Buf instead of Str.

method use-certificate-file
---------------------------

```raku
method use-certificate-file(OpenSSL:, Str $file)
```

Assings a certificate (from file) to the SSL object.

method use-privatekey-file
--------------------------

```raku
method use-privatekey-file(OpenSSL:, Str $file)
```

Assings a private key (from file) to the SSL object.

method check-private-key
------------------------

```raku
method check-private-key(OpenSSL:)
```

Checks if private key is valid.

method shutdown
---------------

```raku
method shutdown(OpenSSL:)
```

Turns off the connection.

method ctx-free
---------------

```raku
method ctx-free(OpenSSL:)
```

Frees C's SSL_CTX struct.

method ssl-free
---------------

```raku
method ssl-free(OpenSSL:)
```

Frees C's SSL struct.

method close
------------

```raku
method close(OpenSSL:)
```

Closes the connection.

Unlike .shutdown it calls ssl-free, ctx-free, and then it shutdowns.

TOOLS
=====

Public key signing tools.

OpenSSL::RSATools
-----------------

```raku
use OpenSSL::RSATools;

my $pem = slurp 'key.pem';
my $rsa = OpenSSL::RSAKey.new(private-pem => $pem);
my $data = 'as df jk l';
my $signature = $rsa.sign($data.encode);
my $rsa = OpenSSL::RSAKey.new(public-pem => $public);
if $rsa.verify($data.encode, $signature) { ... }
```

OpenSSL::CryptTools
-------------------

Symmetric encryption tools (currently only AES256/192/128 encrypt/decrypt)

```raku
use OpenSSL::CryptTools;

my $ciphertext = encrypt("asdf".encode,
                         :aes256,
                         :iv(("0" x 16).encode),
                         :key(('x' x 32).encode));
my $plaintext = decrypt($ciphertext,
                        :aes256,
                        :iv(("0" x 16).encode),
                        :key(('x' x 32).encode));
```

OpenSSL::Digest
---------------

```raku
use OpenSSL::Digest;

my Blob $digest = md5("filename".IO);    # IO::Path object
my Blob $digest = md5(Blob.new(1,2,3));  # Blob object
my Blob $digest = md5("foo bar");        # coercible to string

say md5-hex("foo bar");  # 327b6f07435811239bc47e1544353273
```

Digest Functions exported as subroutines. Takes either an `IO::Path` object of a path of which to create a digest, or a `Blob` object, or an object that can be coerced to a string. A `Blob` is always returned.

  * md5

  * sha1

  * sha224

  * sha256

  * sha384

  * sha512

These subroutines have hexified counterparts with the same name, but postfixed with "-hex", which return a string (lowercase hexadecimal characters) representation of the digest.

  * md5-hex

  * sha1-hex

  * sha224-hex

  * sha256-hex

  * sha384-hex

  * sha512-hex

OpenSSL::Digest::MD5
--------------------

OO-Interface supporting incremental digesting

```raku
use OpenSSL::Digest::MD5;

my $md5 = OpenSSL::Digest::MD5.new; # Create fresh object
$md5.add('abc');                    # pass in Str or Blob
$md5.add('def');                    # Add some more data
my $digest = $md5.hash;             # Blob hash (and reset)
$md5.addfile('myfile');             # Read a file
my $hexdigest = $md5.hex;           # hex hash  (and reset)
```

CAVEATS
=======

MacOS
-----

Many native libraries on MacOS are installed with the `brew` command line interface. For this module one would typically have to do a `brew install openssl`.

The use of native libraries is slightly more complicated on the MacOS operating system than on other operating systems. This generally means that a symlink needs to be installed in a trusted filesystem location. If the [`MacOS::NativeLib`](https://raku.land/zef:lizmat/MacOS::NativeLib) distribution is installed, then these symlinks will be automatically created when this module is built.

SEE ALSO
========

[IO::Socket::SSL](IO::Socket::SSL)

AUTHORS
=======

  * Filip Sergot

  * Elizabeth Mattijsen

Source can be located at: https://github.com/raku-community-modules/OpenSSL . Comments and Pull Requests are welcome.

COPYRIGHT AND LICENSE
=====================

Copyright 2014 - 2022 Filip Sergot

Copyright 2023 - 2026 The Raku Community

This library is free software; you can redistribute it and/or modify it under the MIT License.

