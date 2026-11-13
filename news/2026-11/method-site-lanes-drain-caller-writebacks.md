# Method-site lanes no longer switch off after a leaked caller writeback

The method-table lanes in front of `CallMethodMut` (`.WHICH`, `.defined`,
`.elems`, the built-in iterator protocol, ...) declined every call while the
interpreter's retain-on-miss caller-var writeback list was non-empty. That list
keeps names no live frame owns: a module-scope `my` written from a
`Lock.protect` block, or a bound index ref, as in Monad's type cache. So one
`Monad::Maybe.some(1)` on the mainline turned the lanes off for the rest of the
mainline, and `Any.WHICH` there cost ~18.2k instructions instead of ~7.3k.

The lanes now answer and then drain the list exactly as the full path's
post-call drain does, so a writeback the current frame owns still reaches its
slot and an unclaimable one no longer costs every later call the full probe
chain.
