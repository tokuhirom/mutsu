# Test::Util::ServerPort 

Get a free server port for testing with

![Build Status](https://github.com/jonathanstowe/Test-Util-ServerPort/workflows/CI/badge.svg)

## Synopsis

```raku

use Test::Util::ServerPort;

my $port = get-unused-port();

# .. start some server with the port


```

## Description

This is a utility to help with the testing of TCP server software.

It exports a single subroutine ```get-unused-port``` that will return
a port number in the range 1025 - 65535 (or a specified range
as an argument,) that is free to be used by a listening socket. It
checks by attempting to ```listen``` on a random port on the range
until it finds one that is not already bound.

## Installation

Assuming you have a working Rakudo installation you should be able to install this with *zef* :

    # From the source directory
   
    zef install .

    # Remote installation

    zef install Test::Util::ServerPort

## Support

Suggestions and patches that may make it more useful in your software
are welcomed via github at:

https://github.com/jonathanstowe/Test-Util-ServerPort

## Licence

This is free software.

Please see the [LICENCE](LICENCE) file in the distribution for details.

© Jonathan Stowe 2016 - 2021
