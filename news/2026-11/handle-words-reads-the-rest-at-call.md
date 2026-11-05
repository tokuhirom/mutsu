# `IO::Handle.words` reads the rest of the handle when called

`$handle.words` (and `words($handle)`) returned a deferred Seq that read the
file only when consumed, so `$handle.tell` stayed at 0 after `.words.kv`.
Rakudo consumes the whole decoder up front (`tell` is the file size,
`.is-lazy` is False); mutsu now reads the remaining words at the call. `lines`
keeps streaming (#16012).
