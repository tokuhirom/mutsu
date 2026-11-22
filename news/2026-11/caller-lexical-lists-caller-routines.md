# `CALLER::LEXICAL::` lists the caller's file-scope subs

```raku
sub n(Str $x) { CALLER::LEXICAL::.AT-KEY('&' ~ $x) // 'none' }
sub b($x) { $x }
say n('b');
```

This printed `none`; rakudo prints `&b` (#17733). The caller stash was built
from the caller frame's env snapshot. A file-scope `sub` is not in that
snapshot: it lives in the routine registry, or in its unit's private table
when it is lexical. The single-name form `CALLERS::<&b>` already resolved
such a routine through the caller's scope (its packages, `GLOBAL`, its unit's
imports and private routines). The stash now lists the routines of that same
scope, each resolved as `CALLERS::<&name>` resolves it. The resolution moved
to `runtime/caller_scope_routines.rs`, which both forms share.
