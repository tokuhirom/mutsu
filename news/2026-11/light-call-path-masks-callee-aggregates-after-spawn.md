# The light call path no longer lets a spawning callee's `%h`/`@a` replace the caller's

A routine with only positional scalar parameters takes the light call path, which
had no `ThreadParamMaskGuard`. If its body declared `my %pars` / `my @p` and spawned
a thread, the callee's entry in the shared bare-name lane answered the caller's
same-named variable after the call returned (`%pars<a>` was lost in favour of the
callee's keys). The light path now installs the same guard as the general call path,
which masks the parameters during the call and re-masks the routine's own aggregate
lexicals on return. Part of #15020.
