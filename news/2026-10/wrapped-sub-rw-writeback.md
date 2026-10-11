# `is rw` arguments of wrapped routines write back to sub-local variables

A routine with a `.wrap` chain (every `is native` sub, since NativeCall rebinds
`$!do`) now receives its `is rw` positional arguments as the caller's boxed
containers, so a wrapper's `|c` capture and a native out-parameter
(`int32 is rw`) write back to a sub-local `my` variable. Index-assigning a
Capture element that holds such a container now writes through it and keeps the
Capture instead of rebuilding it as an Array (#12521).

The same boxing now applies to a routine installed by an `EXPORT` hook as an
`&`-sigiled callable (`Compress::Bzip2::Raw`'s native `BZ2_bzBuffToBuff*`), whose
call site hands over plain values plus the caller's variable names. This is what
made `compressToBlob` / `decompressToBlob` return a buffer of the wrong length.
