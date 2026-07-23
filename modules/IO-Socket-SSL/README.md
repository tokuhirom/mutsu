[![Actions Status](https://github.com/raku-community-modules/IO-Socket-SSL/actions/workflows/linux.yml/badge.svg)](https://github.com/raku-community-modules/IO-Socket-SSL/actions) [![Actions Status](https://github.com/raku-community-modules/IO-Socket-SSL/actions/workflows/macos.yml/badge.svg)](https://github.com/raku-community-modules/IO-Socket-SSL/actions) [![Actions Status](https://github.com/raku-community-modules/IO-Socket-SSL/actions/workflows/windows.yml/badge.svg)](https://github.com/raku-community-modules/IO-Socket-SSL/actions)

NAME
====

IO::Socket::SSL - interface for SSL connection

SYNOPSIS
========

```raku
use IO::Socket::SSL;
my $ssl = IO::Socket::SSL.new(:host<example.com>, :port(443));
if $ssl.print("GET / HTTP/1.1\r\n\r\n") {
    say $ssl.recv;
}
```

DESCRIPTION
===========

This module provides an interface for SSL connections.

It uses C to setting up the connection so far (hope it will change soon).

METHODS
=======

method new
----------

```raku
method new(*%params) returns IO::Socket::SSL
```

Gets params like:

  * encoding : connection's encoding

  * input-line-separator : specifies how lines of input are separated

for client state:

  * host : host to connect

  * port : port to connect

for server state:

  * localhost : host to use for the server

  * localport : port for the server

  * listen : create a server and listen for a new incoming connection

  * certfile : path to a file with certificates

method recv
-----------

```raku
method recv(IO::Socket::SSL:, Int $n = 1048576, Bool :$bin = False)
```

Reads $n bytes from the other side (server/client).

Bool :$bin if we want it to return Buf instead of Str.

method print
------------

```raku
method print(IO::Socket::SSL:, Str $s)
```

method send
-----------

DEPRECATED. Use `.print` instead

```raku
method send(IO::Socket::SSL:, Str $s)
```

Sends $s to the other side (server/client).

method accept
-------------

```raku
method accept(IO::Socket::SSL:);
```

Waits for a new incoming connection and accepts it.

close
-----

```raku
method close(IO::Socket::SSL:)
```

Closes the connection.

SEE ALSO
========

[OpenSSL](OpenSSL)

EXAMPLE
=======

To download sourcecode of e.g. github.com:

```raku
use IO::Socket::SSL;
my $ssl = IO::Socket::SSL.new(:host<github.com>, :port(443));
my $content = Buf.new;
$ssl.print("GET /\r\n\r\n");
while my $read = $ssl.recv {
    $content ~= $read;
}
say $content;
```

AUTHOR
======

  * Filip Sergot

Source can be located at: https://github.com/raku-community-modules/IO-Socket-SSL . Comments and Pull Requests are welcome.

COPYRIGHT AND LICENSE
=====================

Copyright 2014 - 2022 Filip Sergot

Copyright 2023 - 2025 The Raku Community

This library is free software; you can redistribute it and/or modify it under the MIT License.

