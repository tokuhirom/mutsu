# A module file without `unit` keeps its file-scope lexicals to itself

A module file with no `unit` declarator (the shape of Rakudo's
`Telemetry.rakumod`) declares its routines in `GLOBAL` and its file-scope
`my` variables at the top of the file. mutsu left those variables in the
importing scope's env, so the module's routines found them through whatever
frame called them:

```raku
# M.rakumod
my $count = 0;
sub bump() is export { ++$count }

# script
use M;
sub c() { my $count = 41; bump() }
say c();                                        # 1 (could read the caller's 41)
say (try EVAL q[$count]) // "not visible";      # not visible
```

A caller's same-named lexical answered for the module's variable, a `:=`
rebind in one routine reached its siblings only through the call's return
merge, and the importer could see the module's variables. These variables now
live in a store keyed by the module file itself, the same way a `unit
module`'s already did under its package, and every routine declared in that
file finds them there (ADR-12529 §3, slice 24).
