# Dotted postcircumfix subscripts interpolate in strings

`$m.<x>`, `%h.<a>`, and `@a.[1]` call the same postcircumfix operator as
`$m<x>`, `%h<a>`, and `@a[1]` via method syntax. String interpolation already
handled the bare forms, but stopped at the leading `.` for the dotted ones and
emitted the subscript text literally: `"$m.<x>"` produced `` "b.<x>" `` instead
of `"b"`.

The interpolation parser now strips a leading `.` before matching a
postcircumfix opener (`<`, `<<`, `«`, `[`, `{`), so the dotted spellings
resolve to the same index expression as their bare counterparts. An ordinary
`.method` call is untouched, since the dot is only stripped when a
postcircumfix opener immediately follows it.

Pinned by `t/collections/subscript/interp-dotted-postcircumfix-subscript.t`.

Closes #9804.
