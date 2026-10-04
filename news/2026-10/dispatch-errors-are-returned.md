# Dispatch and smartmatch errors are returned, not parked

`Interpreter::pending_dispatch_error` is gone (ADR-10779 D3). Two unrelated error channels
shared that one field.

Routine resolution now returns its error. The resolvers used to answer "no candidate" and
leave `X::Multi::Ambiguous`, a `where` clause that died, or a proto's refusal in the field.
Every caller then had to know whether to take it, clear it first, or save and restore it
around its own resolve. They now return `Result<Option<def>, RuntimeError>`. The call that
dispatches raises the error, and a mere probe ignores it.

Smartmatch now reports an exception raised while matching (a user `ACCEPTS` that dies, a
Pair matcher naming a missing method) through an error sink passed down its recursion.
Before, only the `~~` operator looked at the field, so `grep` and `first` swallowed the
exception and the next unrelated `~~` raised it:

```raku
class M { method ACCEPTS($) { die "accepts boom" } }
try { my @r = (1, 2).grep(M.new) };   # mutsu: no error
say 3 ~~ Int;                          # mutsu: died with "accepts boom"
```

`~~`, `when`, `grep` and `first` now raise it where it happens, as raku does.
