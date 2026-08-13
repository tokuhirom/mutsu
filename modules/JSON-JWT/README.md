[![Actions Status](https://github.com/raku-community-modules/JSON-JWT/actions/workflows/linux.yml/badge.svg)](https://github.com/raku-community-modules/JSON-JWT/actions) [![Actions Status](https://github.com/raku-community-modules/JSON-JWT/actions/workflows/macos.yml/badge.svg)](https://github.com/raku-community-modules/JSON-JWT/actions)

NAME
====

JSON::JWT - JSON Web Token (JWT) implementation for Raku

SYNOPSIS
========

```raku
use JSON::JWT;

my $jwt = JSON::JWT.encode($data, :alg<none>);             # no encryption
my $jwt = JSON::JWT.encode($data, :alg<HS256>, :$secret);  # HS256 encryption
my $jwt = JSON::JWT.encode($data, :alg<RS256>, :$pem);     # RS256 encryption

my $data = JSON::JWT.decode($jwt, :alg<none>);             # no encryption
my $data = JSON::JWT.decode($jwt, :alg<HS256>, :$secret);  # HS256 encryption
my $data = JSON::JWT.decode($jwt, :alg<RS256>, :$pem);     # RS256 encryption
```

DESCRIPTION
===========

JSON::JWT provides a class with an implementation of the JSON Web Token (JWT) standard, with support for `HS256` and `RS256` encryption, or no encryption.

AUTHOR
======

Andrew Egeler

Source can be located at: https://github.com/raku-community-modules/JSON-JWT . Comments and Pull Requests are welcome.

COPYRIGHT AND LICENSE
=====================

Copyright 2017 - 2018 Andrew Egeler

Copyright 2019 - 2024 Raku Community

All files in this repository are licensed under the terms of Create Commons License; for details please see the LICENSE file

