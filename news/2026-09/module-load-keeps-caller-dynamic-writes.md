# A module load no longer throws away its writes to the caller's dynamics

A module that wrote a **dynamic variable belonging to the importing scope** had
that write discarded. Both halves of a load were affected — the mainline and
`sub EXPORT`:

```raku
# lib/Mainline.rakumod
$*PACKAGE_LOADED++;

# lib/Bar.rakumod
sub EXPORT (|) { $*PACKAGE_LOADED++; BEGIN Map.new }
```

```
$ raku  -I lib -e 'my $*PACKAGE_LOADED = 0; EVAL q[use Mainline]; say $*PACKAGE_LOADED'
1
$ mutsu -I lib -e 'my $*PACKAGE_LOADED = 0; EVAL q[use Mainline]; say $*PACKAGE_LOADED'
0        # and the same 1-vs-0 for the `sub EXPORT` shape
```

An ordinary sub call propagates such a write correctly
(`pop_caller_env_with_writeback`), and so does a plain `EVAL`, so the loss was
specific to the module-load boundary.

## Root cause

Both halves restore the caller's env **wholesale** after running module code,
and that restore is what reverted the dynamic:

- `Interpreter::apply_module_export` snapshots `self.env` before the `EXPORT`
  call and assigns it straight back afterwards. Dropping EXPORT's own
  params/locals is deliberate — the call's scalar return-merge writes them into
  whatever env is current at return time, where a stale entry would shadow a
  capture the returned sub closes over — but the same assignment reverted every
  `$*` write, which is the *caller's* state, not EXPORT's.
- The module-load path in `run_modules.rs` restores every plain caller binding
  (`saved_plain_env`) for the same kind of reason: an `our $x` in a module can
  replace a same-named caller lexical's env entry before the module's exports
  are installed. A mainline `$*x` write went the same way.

## The fix

Both restores are now narrowed by one predicate,
`crate::env::is_dynamic_var_env_key` (memoized on `Symbol`):

- `run_modules.rs` skips dynamic keys when replaying `saved_plain_env`. A
  dynamic the module declared for itself never entered that snapshot — it is a
  compunit lexical, already extracted into `unit_lexicals` — so it still dies
  with the load.
- `apply_module_export` restores through a new
  `restore_caller_env_keeping_dynamics`, which carries a post-call value back
  only for a dynamic key the caller **already had**. EXPORT's own lexicals are
  dropped exactly as before.

`rerun_module_export` needed one more thing. Raku runs `sub EXPORT` on every
import, not once per process, and mutsu re-runs it against the module scope
remembered from the *first* load — so a second `use` read that load's stale
`$*PACKAGE_LOADED`, incremented it back to the value the caller already had,
and the carry-back correctly saw no change. Dynamics are dynamic-scope, so the
importer's live bindings are now overlaid onto the remembered module env before
the re-run (`overlay_caller_dynamics`). `my $*X = 0; EVAL q[use Bar]; EVAL q[use
Bar]` now says 2, as rakudo does.

## Consequence for the `if` battery

`modules/if/`'s upstream `t/if.rakutest` counts loads exactly this way
(`my $*PACKAGE_LOADED = 0; EVAL $code; is $*PACKAGE_LOADED, 1`), so three of its
five assertions failed on this alone and the battery shipped with an empty
whitelist. The file now passes 5/5 and is whitelisted in
`batteries-whitelist.txt`, taking the `if` suite from 0/1 to 1/1 files in the
release gate.

Pinned by `t/modules/module-load-writes-caller-dynamic.t` (6 assertions, green
under rakudo too), which also pins the two directions the narrowing must not
break: an outer dynamic binding of the same name stays untouched, and a `sub
EXPORT` lexical still does not leak into the importer.

Fixes [#8229](https://github.com/tokuhirom/mutsu/issues/8229).
