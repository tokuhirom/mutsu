# `OUR::`/`GLOBAL::` stash enumeration still carries builtin noise

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Language/syntax.rakudoc:429`). **Partially fixed** — see "What was fixed"
below. What remains is one specific, higher-blast-radius sub-problem, described
here so the next person starts from measurement rather than from scratch.

## Original repro

```raku
my $foo::bar = 1;
say OUR::.keys;           # raku: (foo)
say OUR::foo.HOW          # raku: Perl6::Metamodel::PackageHOW.new
```

## What was fixed

Two of the three failures are closed and pinned by
`t/eval-compunit-introspection.t` (which passes verbatim under `raku`):

1. **A qualified declaration now creates a sub-package stash member, not a flat
   key.** `package_stash_value`'s env scan inserted the whole tail as one
   symbol, so `my $foo::bar = 1` produced a literal `foo::bar` member and no
   `foo`. It now mirrors what the class loop in the same function already did:
   a tail that is itself qualified contributes its *head* as a package value,
   once. `OUR::.keys` contains `foo` (not `foo::bar`) and
   `OUR::foo.WHO.keys` is `($bar)`, matching raku.

2. **`OUR::foo` resolves as a package instead of dying.** The qualified-bareword
   fallback in `vm_var_get_ops.rs` treated any lowercase last segment as a
   routine call, so `OUR::foo` reported
   `Could not find symbol '&foo' in 'OUR'`. It now also asks
   `package_namespace_exists` — is anything stored under `foo::`? — which is
   what makes an *implicitly* created package findable, since `my $foo::bar = 1`
   declares no package anywhere. The same predicate gives such a package
   `Perl6::Metamodel::PackageHOW` instead of the default `ClassHOW`
   (`methods_introspect.rs`), matching raku. Verified this does **not** widen to
   `X` or `IO`, which stay `ClassHOW` because they are registered classes /
   builtin types.

## What remains

`OUR::.keys` at file scope still returns ~58 entries — every builtin class
(`Promise`, `Int`, `Thread`, …) and every dynamic variable (`$*CWD`, `%*ENV`,
`$?FILE`, `$=pod`) — where raku returns only the package's own `our` symbols and
sub-packages. Measured on rakudo v2026.06:

```
my $m = 1; our $o = 2;
MY::.keys     # (!UNIT_MARKER $! $/ $=finish $=pod $?CHECKSUM ... $m $o EXPORT GLOBALish)
OUR::.keys    # ($o)
GLOBAL::.keys # ($o)
```

So `MY::` is roughly right in mutsu already (18 entries, comparable); it is
`GLOBAL::`/`OUR::` that is over-broad, and they are the same stash —
`our_pseudo_stash()` is `package_stash_value(current_package)`, which at file
scope is `GLOBAL`.

**Why this was not fixed here.** The builtin entries come from two loops in
`package_stash_value` (`src/runtime/accessors_stash.rs`): the env scan (dynamic
variables and compile-time magicals) and the `registry().classes` scan (every
builtin class). Narrowing them changes `GLOBAL::` *membership*, which the
symbolic-deref road (`::('Name')`, `GLOBAL::<Name>`, `package_chain_var_fallback`)
reads for lookup and not just for `.keys` — the same map serves both. Doing this
correctly needs enumerable members separated from resolvable ones, or a
user-declared-vs-builtin marker on classes (roles have
`registry().user_declared_roles`; classes have no equivalent). That is a
distinct design decision, not a slice of this ticket.

One promising signal for whoever picks it up: `t/bare-package-symbolic-deref.t`
already asserts `dies-ok { GLOBAL::('Int') }`, i.e. builtin types are *not*
supposed to be reachable through the GLOBAL stash — so the lookup road may not
depend on them being members after all, and the narrowing may be cheaper than it
looks. Measure that first.

**This does not block `pseudo-6c.t`.** `TODO_roast/BLOCKERS.md` records the
`OUR::` cluster (subtests 44, 46-53) as already DONE, and that file still fails
exactly its baseline 14/161 with these changes — its remaining failures are the
CALLER/stash-road cluster, unrelated to stash membership.
