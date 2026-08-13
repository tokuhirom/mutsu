[![Actions Status](https://github.com/raku-community-modules/IO-Socket-Async-SSL/actions/workflows/linux.yml/badge.svg)](https://github.com/raku-community-modules/IO-Socket-Async-SSL/actions) [![Actions Status](https://github.com/raku-community-modules/IO-Socket-Async-SSL/actions/workflows/macos.yml/badge.svg)](https://github.com/raku-community-modules/IO-Socket-Async-SSL/actions) [![Actions Status](https://github.com/raku-community-modules/IO-Socket-Async-SSL/actions/workflows/windows.yml/badge.svg)](https://github.com/raku-community-modules/IO-Socket-Async-SSL/actions)

NAME
====

IO::Socket::Async::SSL - Provides an API like IO::Socket::Async, but with TLS support

SYNOPSIS
========

Client:

```raku
use IO::Socket::Async::SSL;

my $conn = await IO::Socket::Async::SSL.connect('raku.org', 443);
$conn.print: "GET / HTTP/1.0\r\nHost: raku.org\r\n\r\n";
react {
    whenever $conn {
        .print
    }
}
$conn.close;
```

Server (assumes certificate and key files `server-crt.pem` and `server-key.pem`):

```raku
use IO::Socket::Async::SSL;

react {
    my %ssl-config =
      certificate-file => 'server-crt.pem',
      private-key-file => 'server-key.pem';

    whenever IO::Socket::Async::SSL.listen(
      'localhost', 4433, |%ssl-config
    ) -> $conn {
        my $req = '';
        whenever $conn {
            $req ~= $_;
            if $req.contains("\r\n\r\n") {
                say $req.lines[0];
                await $conn.print(
                  "HTTP/1.0 200 OK\r\nContent-type: text/html\r\n\r\n"
                  ~ "<strong>Hello from a Raku HTTP server</strong>\n");
                $conn.close;
            }
        }
    }
}
```

DESCRIPTION
===========

This module provides a secure sockets implementation with an API very much like that of the Raku built-in `IO::Socket::Async` class. For the client case, provided the standard certificate and host verification are sufficient, it is drop-in replacement. The server case only needs two extra arguments to `listen`, specifying the server key and certificate.

As with `IO::Socket::Async`, it is safe to have concurrent connections and to share them across threads.

Client
------

The `connect` method on `IO::Socket::Async::SSL` is used to establish a SSL connection to a server. It requires two positional arguments, which specify the `host` and `port` to connect to. It returns a `Promise`, which will be kept with an `IO::Socket::Async::SSL` instance when the connection is established and the SSL handshake completed.

```raku
my $conn = await IO::Socket::Async::SSL.connect($host, $port);
```

By default, the SSL certificate will be verified, using the default set of accepted Certificate Authorities. The `Promise` returned by `connect` will be broken if verification fails.

Sometimes it is convenient to create a Certificate Authority (CA) and use it to sign certificates for internal use, for example to secure communications between a set of services on an internal network. In this case, the `ca-file` named argument can be passed to specify the certificate authority certificate file:

```raku
my $ca-file = '/config/ca-crt.pem';
my $conn = await IO::Socket::Async::SSL.connect(
  'user-service', 443, :$ca-file
);
```

Alternatively, a `ca-path` argument can be specified, indicating a directory where one or more certificates may be found.

It is possible to disable certificate verification by passing the `insecure` named argument a true value. As the name suggests, **this is not a secure configuration**, since there is no way for the client to be sure that it is communicating with the intended server. Therefore, it is vulnerable to man-in-the-middle attacks.

For control over the ciphers that may be used, pass the `ciphers` argument to `connect`. It should be a string in [OpenSSL cipher list format](https://www.openssl.org/docs/man1.0.2/apps/ciphers.html).

If wishing to view encrypted traffic with a tool such as Wireshark for debugging purposes, pass a filename to the `ssl-key-log-file` name argument. Session keys will be written to this log file, and Wireshark can then be configured to introspect the encrypted traffic (Preferences -> Protocols -> TLS -> (Pre-)-Master-Secret log filename). Note that this key exposure compromises the security of the session!

Server
------

The `listen` method returns a `Supply` that, when tapped, will start an SSL server. The server can be shut down by closing the tap.

Whenever a connection is made to the server, the `Supply` will emit an `IO::Socket::Async::SSL` instance. The `listen` method requires two positional arguments, specifying the `host` and `port` to listen on. Two named arguments are also required, providing the `certificate-file` and `private-key-file`.

```raku
my %ssl-config =
  certificate-file => 'server-crt.pem',
  private-key-file => 'server-key.pem';
my $connections = IO::Socket::Async::SSL.listen(
  'localhost', 4433, |%ssl-config
);

react {
    my $listener = do whenever $connections -> $conn {
        say "Got a connection!";
        $conn.close;
    }

    whenever signal(SIGINT) {
        say "Shutting down...";
        $listener.close;
        exit;
    }
}
```

For control over the ciphers that may be used, pass the `ciphers` named argument to `connect`. It should be a string in [OpenSSL cipher list format](https://www.openssl.org/docs/man1.0.2/apps/ciphers.html).

The following boolean options are also accepted:

  * `prefer-server-ciphers` - indicates that the order of the ciphers list as configured on the server should be preferred over that of the one presented by the client

  * `no-compression` - disables compression

  * `no-session-resumption-on-renegotiation`

Common client and server functionality
--------------------------------------

Both the `connect` and `listen` methods take the following optional named arguments:

  * `enc`, which specifies the encoding to use when the socket is used in character mode. Defaults to `utf-8`.

  * `scheduler`, which specifies the scheduler to use for processing events from the underlying `IO::Socket::Async` instance. The default is `$*SCHEDULER`. There is rarely a need to change this.

The `Supply`, `print`, `write`, and `close` methods have the same semantics as in [IO::Socket::Async](https://docs.raku.org/type/IO/Socket/Async).

Upgrading connections
---------------------

Some protocols use [opportunistic TLS](https://en.wikipedia.org/wiki/Opportunistic_TLS), where the decision to use transport layer security is first negotiated using a non-encrypted protocol - provided negotiation is successful - a TLS handshake is then performed.

This functionality is provided by the `upgrade-server` and `upgrade-client` methods. Note that the socket to upgrade must be an instance of `IO::Socket::Async`. Further, it is important to **stop reading from the socket before initiating the upgrade**, which will typically entail working with the `Tap` directly, something not normally needed in `react`/`whenever` blocks.

Here is an example of using `upgrade-server`:

```raku
my $server = IO::Socket::Async.listen('localhost', TEST_PORT);
react whenever $server -> $plain-conn {
    my $plain-tap = do whenever $plain-conn.Supply -> $start {
        if $start eq "Psst, let's talk securely!\n" {
            # Must stop reading...
            $plain-tap.close;
            # ...so the module can take over the socket.
            my $enc-conn-handshake = IO::Socket::Async::SSL.upgrade-server(
              $plain-conn,
              private-key-file => 't/certs-and-keys/server.key',
              certificate-file => 't/certs-and-keys/server-bundle.crt'
            );
            whenever $enc-conn-handshake -> $enc-conn {
                uc-service($enc-conn);
            }
            $plain-conn.print("OK, let's talk securely!\n");
        }
        else {
            $plain-conn.print("OK, let's talk insecurely\n");
            uc-service($plain-conn);
        }
    }

    sub uc-service($conn) {
        whenever $conn -> $crypt-text {
            whenever $conn.print($crypt-text.uc) {
                $conn.close;
            }
        }
    }
}
```

Here's an example using `upgrade-client`; again, take note of the careful handling of the `Tap`:

```raku
my $plain-conn = await IO::Socket::Async.connect('localhost', TEST_PORT);
await $plain-conn.print("Psst, let's talk securely!\n");

react {
    my $plain-tap = do whenever $plain-conn -> $msg {
        $plain-tap.close;
        my $enc-conn-handshake = IO::Socket::Async::SSL.upgrade-client(
          $plain-conn,
          host => 'localhost',
          ca-file => 't/certs-and-keys/ca.crt'
        );
        whenever $enc-conn-handshake -> $enc-conn {
            await $enc-conn.print("hello!\n");
            whenever $enc-conn.head -> $got {
                print $got; # HELLO!
                done;
            }
        }
    }
}
```

Method reference
----------------

class IO::Socket::Async::SSL
----------------------------

An asynchronous socket with Transport Layer Security. Has an API very similar to the builtin IO::Socket::Async.

### method connect

```raku
method connect(
    Str(Any) $host,
    Int(Any) $port,
    :$enc = "utf8",
    :$scheduler = Code.new,
    Numeric :$version where { ... } = Code.new,
    :$ca-file,
    :$ca-path,
    :$insecure,
    :$alpn,
    Str :$ciphers,
    :$ssl-key-log-file
) returns Promise
```

Establish a TLS connection. Returns a Promise that will be kept with an IO::Socket::Async::SSL instance if the connection is successful, or broken if the connection fails.

### method upgrade-client

```raku
method upgrade-client(
    IO::Socket::Async:D $conn,
    :$enc = "utf8",
    Numeric :$version where { ... } = Code.new,
    :$ca-file,
    :$ca-path,
    :$insecure,
    :$alpn,
    Str :$ciphers,
    Str :$host,
    :$ssl-key-log-file
) returns Promise
```

Upgrade an existing client socket to TLS. This is useful when implementing StartTLS. It is important that the plaintext tap of the asynchronous socket's Supply is closed, so that it can be re-tapped by this module. Returns a Promise that will be kept with an IO::Socket::Async::SSL instance provided the upgrade succeeds, or broken if it fails.

### method listen

```raku
method listen(
    Str(Any) $host,
    Int(Any) $port,
    Int(Any) $backlog = 128,
    :$enc = "utf8",
    :$scheduler = Code.new,
    Numeric :$version where { ... } = Code.new,
    :$certificate-file,
    :$private-key-file,
    :$alpn,
    Str :$ciphers,
    :$prefer-server-ciphers,
    :$no-compression,
    :$no-session-resumption-on-renegotiation
) returns Supply
```

Open a socket on the specified host and port, and start listening for incoming TLS connections. Returns a Supply, upon which successfully established incoming TLS connections will be emitted.

### method upgrade-server

```raku
method upgrade-server(
    IO::Socket::Async:D $socket,
    :$enc = "utf8",
    Numeric :$version where { ... } = Code.new,
    :$certificate-file,
    :$private-key-file,
    :$alpn,
    Str :$ciphers,
    :$prefer-server-ciphers,
    :$no-compression,
    :$no-session-resumption-on-renegotiation
) returns Supply
```

Upgrade an existing server socket to TLS. This is useful when implementing StartTLS. It is important that the plaintext tap of the asynchronous socket's Supply is closed, so that it can be re-tapped by this module. Returns a Supply that will emit IO::Socket::Async::SSL instance provided the upgrade succeeds, or quit if it fails.

### method Supply

```raku
method Supply(
    :$bin,
    :$enc = Code.new,
    :$scheduler = Code.new
) returns Supply
```

Get a Supply of incoming data, either as a byte buffer if the :bin option is passed, or as strings otherwise. Note that strings will, in applicable encodings, be produced in NFG.

### method print

```raku
method print(
    Str(Any) $str,
    :$scheduler = Code.new
) returns Promise
```

Encode a string and send its bytes over the TLS connection. Returns a Promise that will be kept if the data is sent, and broken in the case of an error.

### method write

```raku
method write(
    Blob $b,
    :$scheduler = Code.new
) returns Mu
```

Send the bytes in the passed blob over the TLS connection. Returns a Promise that will be kept if the data is sent, and broken in the case of an error.

### method peer-host

```raku
method peer-host() returns Mu
```

Get the peer (remote) host

### method peer-port

```raku
method peer-port() returns Mu
```

Get the peer (remote) port

### method socket-host

```raku
method socket-host() returns Mu
```

Get the socket (local) host

### method socket-port

```raku
method socket-port() returns Mu
```

Get the socket (local) port

### method native-descriptor

```raku
method native-descriptor() returns Mu
```

Get the socket native descriptor

### method close

```raku
method close() returns Nil
```

Closes the connection. This will await the completion of any outstanding writes before closing.

### method supports-alpn

```raku
method supports-alpn() returns Bool
```

Check if ALPN support is available

Bugs, feature requests, and contributions
-----------------------------------------

Please use GitHub Issues to file bug reports and feature requests. If you wish to contribute to this module, please open a GitHub Pull Request.

Please send an email to the Raku Security Team (security@raku.org) to report security vulnerabilities.

AUTHOR
======

Jonathan Worthington

COPYRIGHT AND LICENSE
=====================

Copyright 2017 - 2024 Jonathan Worthington

Copyright 2024 - 2025 Raku Community

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

