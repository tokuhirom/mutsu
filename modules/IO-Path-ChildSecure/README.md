[![Actions Status](https://github.com/raku-community-modules/IO-Path-ChildSecure/actions/workflows/test.yml/badge.svg)](https://github.com/raku-community-modules/IO-Path-ChildSecure/actions)

NAME
====

IO::Path::ChildSecure - Secure version of IO::Path.child

SYNOPSIS
========

```raku
use IO::Path::ChildSecure;

# good; you get IO::Path
"foo".IO.&child-secure: 'meow';

# still good if 'foo/meow/foo/bar/../' exists; Failure if it doesn't
"foo".IO.&child-secure: 'meow/foo/bar/../meow';

# bad; path isn't a child; you get Failure
"foo".IO.&child-secure: '../';
```

DESCRIPTION
===========

In the Raku Programming Language, [IO::Path.child](https://docs.raku.org/type/IO::Path#method_child) isn't secure, in a sense that it does no checks for whether the resultant path is actually a child of the original path.

This module provides a subroutine that can be used as an alternative that **will** check whether the resultant path is a child of the original path.

EXPORTED SUBROUTINES
====================

child-secure
------------

```raku
"foo".IO.&child-secure: 'meow'; # good; you get IO::Path
"foo".IO.&child-secure: 'meow/foo/bar/../meow'; # still good
"foo".IO.&child-secure: '../';  # bad; path isn't a child; you get Failure

child-secure "foo".IO, '../';  # can also use as a proper sub
```

Appends the given path chunk to the invocant and ensures the resultant path is, in fact, a child of the invocant, by accessing the filesystem and fully-resolving the path. The last chunk of the resultant path does not have to exist for the resolution to succeed.

Will [fail](https://docs.raku.org/routine/fail) with `X::IO::Resolve` if failed to fully resolve the resultant path or with `X::IO::NotAChild` if the resultant path is not a child of the invocant.

SPECIAL NOTES
=============

If you don't need to ensure secureness, use the much-faster core [`IO::Path.add` method](https://docs.raku.org/type/IO::Path#method_add)

AUTHOR
======

Zoffix Znet

COPYRIGHT AND LICENSE
=====================

Copyright 2017-2018 Zoffix Znet

Copyright 2019-2022 Raku Community

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

