# A native `is rw` out-parameter reaches a local of the calling sub

A NativeCall `is rw` scalar passed from a local variable of an ordinary sub
(`my $len = ...; compress($out, $len, ...)`) was written by C but never seen by
the caller: the interpreter-path `call_sub_value` binds its arguments before
pushing a call frame, so the rw writeback slot was baked under the wrong frame
depth and the caller's drain skipped it. The slot is now re-keyed to the
caller's depth. This is what made Compress::Bzip2's `compressToBlob` return a
buffer twice the input size, and fixes Net::Ethereum's `t/12.t`.
