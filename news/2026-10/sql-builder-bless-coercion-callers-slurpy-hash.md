# SQL::Builder suite passes: bless coercion, CALLERS:: frames, hash slurpy flattening

Working the SQL::Builder distribution (locked on #10045) took all 11 of its test files
from 1 passing to 11. Four general fixes:

- `self.bless(...)` now coerces a provided value for a coercion-typed scalar attribute
  (`has Str() $.x`), as `.new` does, and a `Nil` resets such an attribute to the target
  type object (`Str`) rather than the `Str()` coercion type.
- A routine reading `CALLERS::<$*x>` is now marked as observing its caller frame
  (`GetCallersVar` was missing from the `uses_callframe` scan), so it is no longer served
  by the frameless fast call path. Previously only the first call saw the caller.
- `GetCallerVar`/`GetCallersVar` read the decontainerized value, so
  `our $*d = CALLERS::<$*d>` no longer stores the caller's cell into itself (a hang).
- A non-itemized Hash/Map argument flattens into its Pairs under a `*@` slurpy
  (`f({a => 1})` gives `[:a(1)]`), while `$`-held Hashes stay single elements.
