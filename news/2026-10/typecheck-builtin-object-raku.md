# Type-check messages name a Blob, Buf or IO::Path by its real `.raku`

A failed type check names the offending value by its `.raku`, in parentheses
after its type. For a built-in object type that text was the user-class
attribute dump, so the message read `got Blob (Blob.new)` where rakudo says
`got Blob (Blob.new(1,2))`. `IO::Path` had the same problem, giving
`IO::Path.new` instead of rakudo's `IO::Path.new("/tmp",...`.

`Interpreter::type_check_got_repr` now looks for a native handler before that
dump. It tries the pure native method table, which covers `Blob`/`Buf`, and
then the native instance handlers, which cover `IO::Path`. A user `raku` still
wins over both (#10677).

The lazy-Seq half of #10677 is a gap in `.raku` itself: mutsu prints `(...)`
where rakudo reifies a 100-element prefix. That needs the interpreter to run
the Seq's callbacks, so it is filed separately as #10822.
