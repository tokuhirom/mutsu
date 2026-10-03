# HTTP::Tiny: CATCH returns from methods, META6 `ver`, `Blob.bytes` on type objects

A random draw from the ecosystem ledger picked HTTP::Tiny 0.2.6. Its whole
offline suite (7 files rakudo passes) now passes under mutsu, after three
interpreter fixes:

- **A `return` reaching a method through a `try`/`CATCH` boundary returns from
  that method.** Before a `return` crosses a `try` boundary, mutsu checks that
  the routine it targets is still on the call stack. That check only knew
  routine registration ids, and a method invocation runs under a fresh
  per-call id. So when a method's `CATCH` handled an exception raised inside a
  called block or callback, its `return` turned into `X::ControlFlow::Return`
  ("Attempt to return outside of immediately-enclosing Routine"). A block's
  `return` across a `try` inside a method was silently dropped in the same way.
  Method frames now record their invocation id (`RoutineFrame::callable_id`).
- **`$?DISTRIBUTION.meta<ver>` falls back to `version`**, like Rakudo's
  `CompUnit::Repository::Distribution`. `auth` falls back to `authority` and
  then `author`, and `api` defaults to `''`. HTTP::Tiny builds its user-agent
  string from `meta<ver>`, but its META6.json spells only `version`.
- **`Blob.bytes` on a Buf/Blob type object throws
  `X::Parameter::InvalidConcreteness`.** It used to return the byte length of
  the type's name. HTTP::Tiny's test handle returns `Buf[uint8]` at end of
  input, and that value reached OpenSSL's `BIO_write` through NativeCall as a
  12-byte buffer, which crashed the process with SIGSEGV.
