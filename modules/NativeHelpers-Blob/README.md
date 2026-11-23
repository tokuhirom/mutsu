[![Actions Status](https://github.com/raku-community-modules/NativeHelpers-Blob/actions/workflows/linux.yml/badge.svg)](https://github.com/raku-community-modules/NativeHelpers-Blob/actions) [![Actions Status](https://github.com/raku-community-modules/NativeHelpers-Blob/actions/workflows/macos.yml/badge.svg)](https://github.com/raku-community-modules/NativeHelpers-Blob/actions)

NAME
====

NativeHelpers::Blob - NativeCall utilities for work with Blob and CArray

SYNOPSIS
========

```raku
use NativeHelpers::Blob;
```

DESCRIPTION
===========

The `NativeHelpers::Blob` distribution provides a number of subroutines that help in converting native data-structures to Raku objects, and vice-versa.

Right now the support for `Blob` / `Buf` in Raku's `NativeCall` is incomplete. You can use these as arguments for native functions, but not as attributes for `CStruct` or `CUnion`, for example.

In a `CStruct` class, you can use a `Pointer` or a `CArray`, but those don't have the flexibility of a `Blob` or `Buf`, moving data between them is slow, and with big buffers the memory required increases dramatically.

At some point, these problems will be addressed in core, but in the meantime...

EXPORTED SUBROUTINES
====================

pointer-to(positional, :$typed --> Pointer)
-------------------------------------------

Returns a `Pointer` to the contents of the given `Blob` or (native) `Array`.

The type of the returned `Pointer` will be the same of the `Blob` if the named argument `:typed` was specified with a true value, or `void` if not.

Should be noted that the memory is owned by Rakudo, so you must **not** attempt to free it.

carray-from-blob(Blob:D, :$managed --> CArray)
----------------------------------------------

Returns a `CArray` constructed from de contents of the given `Blob`.

If the named argument `:managed` was specified with a true value, the returned `CArray` contains a copy of the original content of the `Blob` and will be managed, otherwise the `CArray` holds a reference to the `Blob` content.

blob-from-pointer(Pointer:D, :$elems!, Blob:U :$type = Buf --> Blob)
--------------------------------------------------------------------

Returns a `Blob` of the type indicated with the named argument `:type` (defaults to `Buf`) constructed from the memory pointed by the given `Pointer` and the required named argument `:elems` to indicate the number of elements.

The type of the `Pointer` should be compatible with the type of the `Blob`.

Please note that the amount of memory copied depends of `$elems` **and** `:type`´s native size.

blob-from-carray(CArray:D, :$size --> Blob)
-------------------------------------------

Returns a `Blob` constructed from the contents of the `CArray`.

When the `CArray` is unmanaged, for example as returned by a native call function, you should include de `:size` argument.

The type of the `Blob` is determined by the type of the `CArray`.

WARNING
=======

This module depends on internal details of the REPRs of the involved types in MoarVM, so it can stop working without notice.

In the same way as when handling pointers in C, you should known what are you doing.

AUTHORS
=======

  * Salvador Ortiz

  * Raku Community

COPYRIGHT AND LICENSE
=====================

Copyright 2016 - 2019 by Salvador Ortiz

Copyright 2026 Raku Community

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

