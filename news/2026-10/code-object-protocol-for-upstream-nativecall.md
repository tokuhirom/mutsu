# Code-object protocol fixes for upstream NativeCall

Running the vendored upstream NativeCall (#11203) relies on several
code-object behaviours that mutsu either lacked or got wrong. Each one is now
a general fix:

- `nqp::bindattr($code, Code, '$!signature', $sig)` gives a routine a new
  signature, and `.signature` and `.arity` answer it from then on. This is
  how `nativecast(Signature, $ptr)` builds its callable.
- `nqp::bindattr` / `nqp::getattr` on a value with roles mixed in reach the
  role attributes in its role cell, for example `$!entry-point` on a routine
  that does `Native`.
- A `$!do` bound on one module's routine no longer runs for a same-named
  routine that another package declares. A local wrapper that shadows a
  needed module's native sub keeps its own body.
- A role body's statics are what the body declares, including a phaser's
  `INIT my $x`. They no longer include every lexical of the scope that
  composed the role. A role method call also restores what it injected, so a
  caller closure's own `|c` capture survives the call.
- The `Method` object a method's traits receive carries its invocant in the
  signature, so its arity counts the invocant, as in Rakudo.

With these fixes, 56 of the 66 `is native` test files pass with the switch
to upstream NativeCall applied (49 before them).
