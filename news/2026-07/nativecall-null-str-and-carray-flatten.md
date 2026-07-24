# Two NativeCall fixes: NULL `Str` arguments, and `CArray.new` list flattening

The release-time battery gate reported OpenSSL's `10-client-ca-file.rakutest`
exiting 139 — a **SIGSEGV**. Two independent NativeCall bugs were behind it; both
are general marshalling defects, not OpenSSL-specific.

## 1. An undefined `Str` argument must be a NULL `char*`

`marshal_arg`'s `CType::Str` arm stringified the value unconditionally, so `Nil`
(or a type object) became a pointer to a 1-byte buffer instead of NULL. A callee
that treats the parameter as a caller-supplied **output** buffer then writes past
it. That is exactly OpenSSL's

```raku
ERR_error_string($e, Nil)   # NULL means "use your own static buffer"
```

which writes up to 256 bytes — aborting mutsu with `realloc(): invalid next
size`. Undefined values now marshal to a null pointer, matching Rakudo.

Pinned by `t/nativecall-null-str-arg.t`, which uses `getcwd(buf, size)` because
its NULL path is both safe and observable: `getcwd(NULL, 0)` allocates and
returns the cwd, while a non-NULL buffer with size 0 fails with `EINVAL`. That
distinguishes NULL from the empty string the old code sent (`Nil.Str` is `""`).

## 2. `CArray[T].new` flattens an Array/List argument

`CArray[uint8].new($path.encode.list, 0)` — the idiomatic way to build a
NUL-terminated C string — produced a **2**-element buffer (the whole list as
element 0, then 0) instead of the 13 elements Rakudo gives. The callee therefore
saw a garbage filename, and OpenSSL's `use-client-ca-file` reported "No such file
or directory" for a file that was right there.

The aggregate constructor flattened `Slip` / `Range` / `Seq` / `LazyList`
arguments but not `Array`/`List`. Checked against rakudo, **only `CArray`**
flattens those: `Array.new(@a, 3)` and `List.new(@a, 3)` keep `@a` as a single
element, so the fix is scoped to `CArray` alone. Pinned by
`t/carray-new-flattens-list.t`.

## Result

`10-client-ca-file.rakutest` goes from SIGSEGV to **7/7 passing**, lifting the
OpenSSL battery baseline from 2/7 to 3/7 in `batteries-whitelist.txt`.
