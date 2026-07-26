# An unimplemented `nqp::` op errors instead of silently becoming a Raku builtin

```raku
use nqp;
say nqp::index("hello", "z");
# nqp:   -1
# mutsu: Nil      <- silently Raku's `index`, not nqp's
```

`call_function_fallback` ends with a package-prefix strip: an unresolved
`Foo::bar(…)` retries as `bar(…)`, which is how a call qualified with a package
mutsu never registered still finds its routine. For `nqp::` that fallback landed
on Raku's same-named builtin — and nqp's ops are *not* the Raku ones.
`nqp::index` returns `-1` for "not found" where Raku's returns `Nil`, and nqp
code branches on exactly that (`!= -1`). The result was a **silent wrong answer**
rather than an error.

An unimplemented `nqp::` op now fails with
`Unsupported nqp:: op: nqp::<name>`. The `nqp::` namespace is reserved and its op
set is documented, so rejecting an unimplemented one is safe. The ops mutsu
really does implement (`nqp::atkey`, `nqp::atpos`, `nqp::ordat`,
`nqp::gethostname`, `nqp::bindattr`) are matched earlier under their full name
and are unaffected, as is `use nqp` itself.

The guard is deliberately scoped to `nqp::`. The same shape exists more widely —
`Foo::Bar::index("hello", "l")` also reaches the builtin where raku says
`Could not find symbol '&index' in 'GLOBAL::Foo::Bar'` — but the short-name retry
is load-bearing there, and the obvious guard does not work: `index` is dispatched
by a hand-written arm in `call_function`, not via `BUILTIN_FUNCTION_NAMES`, so
`is_builtin_function` does not even recognise it. That case is recorded in
`todo/tickets/nqp-op-aliasing-and-sha1.md` rather than changed blind.

## Why this, and not an `nqp::` layer

The same ticket records the measurement that settled the strategy question.
`nqp::` appears in 7.6% of cached fez dists but 20.4% of reverse-dependency
weight — except that weight is dominated by `JSON::Fast` (1439 reverse-deps),
which **mutsu already bundles**, so implementing its 42 missing ops would change
nothing observable. And per dist the op set is a threshold function: 80% of a
module's ops still leaves it dead, and the remaining fifth includes thunk-taking
control ops (`nqp::if`/`while`/`stmts`, which cannot be builtins) and a null
sentinel with no `Value` representation. Bundling the few nqp-heavy hub modules
is much cheaper than the layer.

The one genuine `nqp::` demand the survey turned up is **`nqp::sha1`**, which
the *vendored zef* calls from `Zef::Distribution.id` and its source-path
computation — i.e. it sits on the mzef critical path. mutsu has no SHA-1 at all
yet, so that is its own small piece of work.

Pinned by `t/nqp-unimplemented-op-errors.t` (7 subtests: an implemented op still
working, three unimplemented ops failing and naming themselves, a qualified user
sub still resolving, a non-`nqp` qualified call still falling back, and `use nqp`
still loading).
