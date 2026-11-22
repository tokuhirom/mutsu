# `Failure.Capture` and `Signature.Capture` are rows

Rakudo declares `Capture` on `Failure` and `Signature`. A handled Failure, or
either type object, dies with `X::Cannot::Capture` naming the value by its
`.gist` (`` Cannot unpack or Capture `(HANDLED) x` ``, `` `($a)` ``,
`` `(Signature)` ``) and explains how to write a `Capture`. An unhandled
Failure throws its own exception. mutsu raised a bare
`Cannot unpack or Capture Failure` in every case.

Both are now method-table rows (ADR-11276), and the cascade's fallback raises
the same error. A Failure's `.mess` also renders a wrapped built-in exception by
its message (`f not yet implemented. Sorry.`), where before it showed the
object (`X::NYI()`).
