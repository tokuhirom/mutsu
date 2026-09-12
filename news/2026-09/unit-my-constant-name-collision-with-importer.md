# An importer's `my @x` no longer reads empty against a `my constant @x` of the same name

A `unit module`'s body runs against the loading scope's `env`, so a
file-scope `my` lexical needs protection from a same-named caller variable:
`unit_lexicals` (consulted before `env`) has always given plain `my`
declarations that protection. `my constant` was excluded from that store —
only a bare `constant`/`our constant` gets a package-qualified global that
happens to sidestep the bare-env-key collision, so a `my constant @x` was
reachable only through the shared bare env key until it was removed a few
statements later during import cleanup.

In that window, an importer's own `my @x`/`my %x`, declared *after*
`use`-ing the module, shared the identical bare key. Calling the module's
own accessor for that name from the importer's declaration read back the
importer's not-yet-initialized (empty) declaration instead of the module's
constant:

```raku
# lib/Coll2.rakumod
unit module Coll2;
my constant @constarr = 'a', 'b', 'c';
our sub get-const() is export { @constarr }
```

```raku
use Coll2;
my @constarr = get-const;   # mutsu: (), raku: (a b c)
```

`import_module` (`src/runtime/run_modules.rs`) now also copies each unit
compunit's own `constant`/enum name into `unit_lexicals`, sourced from
`module_scope_names` (which still holds the value at that point) rather than
from `env` (whose bare key gets removed a few lines later) to avoid an
ordering hazard. `module_scope_names` itself is left untouched, so the
existing bare-`constant`/`my constant` own-routine reads keep working
exactly as before.

This was the last blocker for the `Locale::US` distribution's test suite,
whose module declares `my constant @codes`/`my constant @states` lookup
tables that collided with the test file's own same-named `my @states`/`my
@codes`.

Fixes #8027.
