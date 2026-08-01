# NativeCall cannot be vendored the way Pod::To::Text was — measured 2026-08-01

`Pod::To::Text` moved from native provision to the genuine rakudo core module in
one PR (#5644, `docs/batteries/pod-to-text.md`), and the obvious next question is
whether `NativeCall` follows. **It does not.** This file records the measurement
so nobody has to re-derive it, and states what would have to become true first.

Decision: keep providing `NativeCall` natively
(`src/runtime/nativecall*.rs`, 1782 lines). Revisit only against the conditions
in "When to reopen" below.

## The measurement

Upstream is rakudo 2026.06 `lib/NativeCall.rakumod` + `lib/NativeCall/`.

| file | lines | `nqp::` refs |
| --- | --- | --- |
| `NativeCall.rakumod` | 723 | 119 |
| `NativeCall/Dispatcher.rakumod` | 300 | 108 |
| `NativeCall/Types.rakumod` | 460 | 81 |
| `NativeCall/Compiler/GNU.rakumod` | — | 27 |
| `NativeCall/Compiler/MSVC.rakumod` | — | 0 |
| **total** | **1483** | **308** |

For scale, `Pod::To::Text` is 168 lines with **zero** `nqp::` references and zero
`use` statements.

Distinct `nqp::` ops required: **76**. Ops mutsu answers today: **15**
(`atkey atpos bindattr bindpos_i bindpos_n box_i chars decont elems istype
not_i p6box_i setelems stmts unbox_i`). **61 are missing.**

## Four independent blockers, not one

**1. It depends on the NQP compiler itself.** Line 2 of `NativeCall.rakumod`:

```raku
use QAST:from<NQP>;
```

NativeCall builds the body of each `is native` sub as a QAST tree and hands it to
the VM. mutsu has no NQP, no QAST, and no `:from<NQP>` foreign-language loader.

**2. `NativeCall::Dispatcher` is written against MoarVM's dispatch programs.**
`nqp::syscall` ×18, `nqp::track` ×16, `nqp::guard` ×6, `nqp::delegate` ×5,
`nqp::register` ×3, `nqp::captureposarg` ×7. These expose the VM's *internal*
dispatch machinery (guard on an argument's type, delegate to another dispatcher,
register a dispatch program) to Raku code. mutsu's VM has no equivalent surface,
and inventing one is a core-design change, not a module port.

**3. The parser rejects `NativeCall::Types` outright.** It does not even reach
the `nqp::` question:

```
$ mutsu -I <vendored> -e 'use NativeCall::Types;'
Failed to parse module 'NativeCall::Types':
X::Syntax::Malformed: Malformed my (did you mean to declare a sigilless \var or $var?)
```

Bisected to line 54 — two missing parser features, both general:

```raku
our class void is repr<Uninstantiable> { }                      # `is repr<...>` trait
our native long is Int is ctype<long> is repr<P6int> { }        # `native` package declarator
```

`native` is a *package declarator* peer to `class`/`role`/`grammar` that rakudo
carries for NativeCall's sake, and `is repr<...>` selects the underlying
representation (`P6int`, `CPointer`, `CArray`, `CStruct`, `CUnion`, ...).

**4. The op set is a threshold function.** Established by measurement in
`news/2026-07/nqp-op-layer-measured-and-rejected.md`: implementing 80% of a
module's ops still leaves it dead, which is why a demand-driven "one op at a
time" plan does not converge. 61 missing ops here span pure data ops, native
typed arrays, control structures that take *thunks* (cannot be builtins — needs
compiler lowering), and representation/meta ops needing P6opaque storage and a
null sentinel distinct from `Nil`/`Any`.

## Why the stakes are higher than for Pod::To::Text

`use NativeCall` is live infrastructure: **33 files across the bundled
batteries** depend on it (OpenSSL, DBIish, Crypt::Random, IO::Socket::SSL). A
half-working replacement does not degrade gracefully — it takes the TLS stack and
the database layer with it. `Pod::To::Text` had exactly one consumer and a
5-subtest roast file.

## How the other native providers compare (same survey)

Measured the same day, so "which provider is next" does not have to be guessed
again. Full table in `docs/batteries/pod-to-text.md`:

| module | lines | `nqp::` | ops (missing) | parses? | verdict |
| --- | ---: | ---: | --- | --- | --- |
| `Pod::To::Text` | 168 | 0 | 0 (0) | yes | done (#5644) |
| `Test` | 953 | 90 | 11 (9) | yes, and **loads** | reachable — `todo/tickets/vendor-real-test-module.md` |
| `experimental` | 260 | 10 | 6 (4) | no | parser gap + `nqp::getcomp` |
| `newline` | 5 | 0 | 0 (0) | yes | pointless to move (`$?NL` stays `Nil`: the `EXPORT::` package mechanism is unimplemented) |
| **`NativeCall`** | **1483** | **308** | **76 (61)** | **no** | **this file** |

`NativeCall` is the outlier by an order of magnitude on every column.

## Salvageable pieces (worth doing on their own merits)

Nothing here is blocked on the NativeCall decision; each is an ordinary
compatibility gap:

- **`is repr<...>` trait** — the parser should accept and record it. Real dists
  use it independently of NativeCall.
- **`native` package declarator** — same.
- Neither makes NativeCall work. Do them because they are missing Raku, not as
  step 1 of a port.

## When to reopen

Reopen only if *all* of these become true, and then re-measure before writing
code:

1. mutsu grows an NQP/QAST surface, or upstream NativeCall stops needing one.
2. mutsu's VM exposes a dispatch-program-equivalent API (guard/track/delegate).
3. The 61-op gap has closed for other reasons.

Short of that, `NativeCall` stays on BATTERIES.md rung 3 — and unlike the old
`Pod::To::Text` situation, that is a *justified* use of the rung, which
BATTERIES.md §1 asks to be recorded.
