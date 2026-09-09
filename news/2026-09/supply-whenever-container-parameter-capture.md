# `supply whenever` keeps each `@`/`%` parameter binding

A `supply whenever` body now retains the array or hash parameter belonging to
the `supply` invocation that created it. A later invocation with the same
parameter name no longer overwrites the earlier pipeline's captured binding.
This fixes [#7661](https://github.com/tokuhirom/mutsu/issues/7661).
