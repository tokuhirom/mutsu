# `is rw` routines return a container

An `is rw` routine can now hand its caller a writable container reached through
its own parameters, and the caller's assignment writes through it:

```raku
sub g(\c) is rw { return-rw c<a> }
my %h; g(%h) = 1;                    # %h is {:a(1)}
```

Before this, mutsu had no lvalue return at all. `assign_named_sub_lvalue_with_values`
never called the routine: it took the callee's **AST tail expression**
(`rw_sub_target_expr` — literally the last `Stmt::Expr`/`Stmt::Return` of the
body) and re-evaluated that expression *in the caller's frame*. That reproduces
the syntax of the common one-liner and cannot express the semantics — the
caller's frame has no binding for the callee's parameters, `return-rw <non-Var>`
was not recognized at all, and a computed tail has no single static expression
to re-interpret. All three shapes died with `X::Assignment::RO: sub 'g' is not
rw`; the method form died with an unrelated "I cannot be parameterized" because
the assignment fell through to the legacy `$obj.name($value)` setter convention.

The design is recorded as
[ADR-0059](../../docs/adr/0059-is-rw-routines-return-a-container.md). Every
piece it needs already existed — it was simply never connected to the `is rw`
return path:

- **Production.** `return-rw <expr>` compiles its operand in the same
  container-producing mode a `:=` bind RHS uses, so a subscript yields the
  element's shared `ContainerRef` cell, or the deferred `HashEntryRef` token for
  a key that does not exist yet (so a *getter* built on the same routine still
  does not vivify, while a write walk-creates the path). Container mode also
  reaches the arguments of a nested call inside the operand, because a recursive
  path-addressing routine's descent runs through them.
- **Transport.** Two latent bugs severed the chain and are fixed: the subscript
  *index* was itself compiled in container mode (so `c{@s[0]}` passed a
  `ContainerRef` where the key was wanted), and an ordinary `GetLocal` resolved a
  deferred bind token to `Any`. The subscript *target* now reads through a new
  `OpCode::GetLocalDeferred`, which is `GetLocal` minus that one resolution step.
- **Consumption.** One helper, `assign_lvalue_container`, writes through a
  `Proxy` (STORE), a `ContainerRef` (the shared cell) or a `HashEntryRef`
  (walk-create the path). Both the sub and the method lvalue paths call the
  routine first and use it; the method path also handles a **type-object
  invocant** (`Crane::In.in(...) = $v`), which the instance-only paths rejected.

The caller-side tail re-interpretation is demoted, not left as a peer: the
routine always runs now, and the old path is consulted only when the routine
returned a plain value — which happens for exactly one shape, a bare
variable/attribute tail (`sub f() is rw { $x }`). ADR-0059 Slice 2 compiles that
to a container return too and deletes the old mechanism.

Two shapes that were independently broken now work as a side effect: a `:=`
chain through a variable holding a deferred token (`my $x := %h<a>; my $y :=
$x<b>; $y = 3`), and `=:=`/subscript compilation no longer mis-treats a computed
index as part of the bind chain.

Pinned by `t/is-rw-lvalue-container-return.t` (15 subtests, byte-identical
output under `raku`).

## Effect on the TOML battery

`Crane` — the sole dependency of `Config::TOML` — is built entirely on this
shape, and every `Crane.set` used to silently do nothing. The ticket's headline
repro now matches `raku` exactly:

```raku
use Crane;
my %h; Crane.set(%h, :path["a","b"], :value(1)); say %h.raku;
# was {}, now {:a(${:b(1)})} — same as raku
```

Crane's upstream suite moves from 263 to 280 passing subtests (188 → 176
failing); `t/set.rakutest` goes 1 → 9 passing, `t/in.rakutest` 5 → 9 and
`t/get.rakutest` 32 → 37. `Config::TOML`'s goes from 77 to 132 passing
assertions, and many more assertions now *run* at all — files that used to abort
on the first `from-toml` reach the end, so `t/grammar-actions/01-primitives`
executes 142 assertions where it previously reached 7.

File granularity is still 3/15 and 0/19, because the remaining failures are
*other* missing features. Chiefly, the deferred vivification token is hash-only:
a positional step in a deferred chain vivifies a `Hash` keyed `"0"` where an
`Array` belongs, which is the array twin of what this change fixed for hashes
and disables all of Crane's `Positional` candidates
(`todo/deep/deferred-vivification-token-is-hash-only.md`). `Config::TOML`'s own
remaining failures are grammar/regex-level and independent.
