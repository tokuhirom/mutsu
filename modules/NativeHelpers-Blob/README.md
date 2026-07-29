# NativeHelpers::Blob

## Introduction

Right now the support for `Blob`/`Buf` in Perl 6's NativeCall is incomplete.
You can use these as arguments for native functions, but not as attributes for
`CStruct` or `CUnion`, for example.

In a `CStruct` class, you can use a `Pointer` or a `CArray`, but those don't have the
flexibility of a `Blob` or `Buf`, moving data between them is slow, and with
big buffers the memory required increases dramatically.

At some point, these problems will be addressed in core, but in the meantime...

## Usage

    use NativeHelpers::Blob;

## Exported functions

### multi sub pointer-to(Blob:D, :$typed -> Pointer)

Returns a `Pointer` to the contents of the `Blob`.

The type of the returned `Pointer` will be the same of the `Blob` if `:typed` was used
or `void` if not.

Should be noted that the memory is owned by Rakudo, so you must not attempt
to free it.

### multi sub pointer-to(array:D, :$typed -> Pointer)

Returns a `Pointer` to the contents of the native `array`.

The type of the returned `Pointer` will be the same of the `array` if `:typed` was used
or `void` if not.

Should be noted that the memory is owned by Rakudo, so you must not attempt
to free it.

### multi sub pointer-to(CArray:D, :$typed -> Pointer)

Returns a `Pointer` to the contents of the native `CArray`.

The type of the returned `Pointer` will be the same of the `CAarray` if `:typed` was used
or `void` if not.

Should be noted that the memory is owned by Rakudo, so you must not attempt
to free it.

### sub carray-from-blob(Blob:D, :$managed)

Returns a `CArray` constructed from de contents of the `Blob`.

If `:managed` was given, the returned `CArray` contains a copy of the original
content of the `Blob` and will be managed, otherwise the `CArray` holds a
reference to the `Blob` content.


### sub blob-allocate(Blob:U \blob, $elems)

Rakudo 2016.03+ provides `Blob.allocate` to create a Blob/Buf with a pre-allocated
initial size.

For older ones you can use this subroutine for the same effect. It create a `blob`
with `$elems` zeroed elements.

### sub blob-from-pointer(Pointer:D, Int :$elems!, Blob:U :$type = Buf)

Returns a `Blob` of type `:type` constructed from the memory pointed by the
given `Pointer` and the following `:elems` elements.

The type of the `Pointer` should be compatible with the type of the `Blob`,

Please note that the amount of memory copied depends of `$elems` **and** `:type`´s
native size.

### sub blob-from-carray(CArray:D, Int :$size)

Returns a `Blob` constructed from the contents of the `CArray`.

When the `CArray` is unmanaged, for example as returned by a native call function, you
should include de `:size` argument.

The type of the `Blob` is determined by the type of the `CArray`.

## WARNING
This module depends on internal details of the REPRs of the involved types in MoarVM,
so it can stop working without notice.

In the same way as when handling pointers in C, you should known what are you doing.

This is an early release to allow the interested people the testing and
discussion of the module: there is some missing features and you should
be aware that the API isn't in stone yet.

## COPYRIGHT

Copyright © 2016 by Salvador Ortiz
