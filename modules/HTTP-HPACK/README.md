[![Actions Status](https://github.com/raku-community-modules/HTTP-HPACK/actions/workflows/linux.yml/badge.svg)](https://github.com/raku-community-modules/HTTP-HPACK/actions) [![Actions Status](https://github.com/raku-community-modules/HTTP-HPACK/actions/workflows/macos.yml/badge.svg)](https://github.com/raku-community-modules/HTTP-HPACK/actions) [![Actions Status](https://github.com/raku-community-modules/HTTP-HPACK/actions/workflows/windows.yml/badge.svg)](https://github.com/raku-community-modules/HTTP-HPACK/actions)

NAME
====

HTTP::HPACK - Implementation of RFC 7541 HPACK: Header Compression for HTTP/2

SYNOPSIS
========

```raku
use HTTP::HPACK;

# Decoding:
my $decoder = HTTP::HPACK::Decoder.new;
my @headers = $decoder.decode-headers($buf);
say "{.name}: {.value} ({.indexing})" for @headers;

# Encoding:
my @headers = 
  HTTP::HPACK::Header.new(
    name  => ':method',
    value => 'GET'
  ),
  HTTP::HPACK::Header.new(
    name     => 'password',
    value    => 'correcthorsebatterystaple',
    indexing => HTTP::HPACK::Indexing::NeverIndexed
  );
my $encoder = HTTP::HPACK::Encoder.new;
my $buf = $encoder.encode-headers(@headers);
```

DESCRIPTION
===========

HPACK is the HTTP/2 header compression algorithm. This module implements encoding (compression) and decoding (decompression) of the HPACK format, as specified in RFC 7541. A HTTP/2 connection will typically have an instance of the decoder (to decompress incoming headers) and an instance of the encoder (to compress outgoing headers).

Notes on specific features
==========================

Huffman compression
-------------------

Decoding of headers compressed using the Huffman codes (set out in the RFC) takes place automatically. By default, the encoder will not apply Huffman compression. To enable this, construct it with the `huffman` named argument set to `True`:

```raku
my $encoder = HTTP::HPACK::Encoder.new(:huffman);
```

Dynamic table management
------------------------

The dynamic table size can be limited by passing the `dynamic-table-limit` named argument when constructing either the encoder or decoder:

```raku
my $decoder = HTTP::HPACK::Decoder.new(dynamic-table-limit => 256);
```

It is also possible to introspect the current dynamic table size:

```raku
say $decoder.dynamic-table-size;
```

The size is computed according to the algorithm in RFC 7541 Section 4.1.

Thread safety
=============

Instances of HTTP::HPACK::Header are immutable and so safe to share and access concurrently. Instances of `HTTP::HPACK::Decoder` and `HTTP::HPACK::Encoder` are stateful (as a result of the dynamic table), and so a given instance may not be used concurrently. This is not a practical problem, since headers may only be processed in the order they are being received or transmitted anyway.

Known issues
============

  * The Huffman code termination handling has not been validated to be completely up to specification, and so may fail to signal errors in some cases where the Huffman code is terminated in a bogus way.

AUTHOR
======

Jonathan Worthington

COPYRIGHT AND LICENSE
=====================

Copyright 2016 - 2023 Jonathan Worthington

Copyright 2024 - 2025 Raku Community

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

