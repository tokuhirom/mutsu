# An accessor argument still loses its container when the callee has no compile-time name

Measured 2026-09-06 against raku v2026.07 and a debug `mutsu` built from `main`
at `1e946f7c8` plus ADR-0067's returned-container-consumers slice, which closed
the *named-sub* spelling of this and left these two.

```raku
class C { has $.v is rw }
my $c = C.new(v => 42);

sub g($y is rw) { $y = 9 }
g($c.v);                                        # raku: 9   mutsu: 9 (fixed)

class Sink { method take($y is rw) { $y = 9 } }
Sink.new.take($c.v);                            # raku: 9   mutsu: dies
my $r = &g; $r($c.v);                           # raku: 9   mutsu: dies
```

| # | Program | raku | mutsu |
|---|---|---|---|
| V1 | `$s.take($c.v)` for `method take($y is rw)` | `9` | dies, `Parameter '$y' expects a writable container` |
| V2 | `$s.take($c.acc)` for `method acc is rw { $!v }` | `9` | `9` — the rw-method producer is unconditional |
| V3 | `my $r = &g; $r($c.v)` | `9` | the same refusal as V1 |
| V4 | `g($c.v)` (named sub) | `9` | `9` — closed by ADR-0067 |

Both remaining rows **refuse loudly**; neither is a silent wrong answer, which
is why they were left rather than forced.

## Root cause

The producer is `OpCode::MarkRwArgRefContext` (ADR-0067): it asks
`try_fast_accessor_read` for the attribute's container, but only after the VM
confirms that the named callee declares a container-binding parameter
(`ParamDef::binds_caller_container`) at that positional index. Emitting the
marker ungated would make every `f($obj.attr)` pay an attribute-slot promotion
plus an MRO walk — the cost the E6 producer measured at ~14% for the
lvalue-invocant spelling — so the gate is keyed on the callee's *name*.

Neither remaining spelling has one:

- **V1, a method-call argument.** The invocant's class is not knowable at
  compile time, so the gate would have to ask "does *any* user method named
  `take` anywhere declare a container-binding parameter at index 0". The
  registry's method table is keyed by `(owner, name)`
  (`Registry::method_entries`, with `owner_method_names` the only reverse index
  and it goes the other way), so there is no name-only lookup to ask, and a full
  scan per marker execution is not affordable.
- **V3, a call through a code variable.** The callee is a value, resolved at run
  time; there is no name at all.

## Why it needs design, not just an edit

The cheap over-approximation for V1 — a program-wide set-only "any user method
declares a container-binding positional parameter" flag, the pattern slice 3a's
`Registry::any_raw_invocant_method` already uses — would hand a container to
*every* accessor-shaped method argument in any program that declares one such
method anywhere. That is a much wider behaviour change than the callee-keyed
gate, and E6's "the single chokepoint that is not single" lesson says the way to
find out what it breaks is a measured sweep, not a guess. The principled
alternative is a name-keyed method index (a `Symbol -> bool` map maintained by
the same `Registry` mutator that maintains `owner_method_names`), which is a
registry change with its own blast radius.

For V3 the only sound route is to move the decision to the *binder*: hand the
call an argument whose provenance the binder can materialize on demand, rather
than deciding at the call site. That is a bigger design question than this
ticket — it is the same shape as the arg-source channel the `is rw` binder
already consults by name.

## Repro

```raku
class C { has $.v is rw }
class Sink { method take($y is rw) { $y = 9 } }
my $c = C.new(v => 42);
Sink.new.take($c.v);
say $c.v;          # raku: 9    mutsu: dies "expects a writable container"
```
