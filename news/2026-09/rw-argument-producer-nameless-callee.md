# An accessor argument keeps its container when the callee has no name — because the callee is already on the stack

ADR-0067's argument producer made `sub g($y is rw) { $y = 9 }; g($c.v)` write
through the attribute's container. Its gate keys on the callee's **name**
(`OpCode::MarkRwArgRefContext`), so two spellings were left open and recorded as
`todo/tickets/rw-argument-producer-needs-a-nameless-callee-gate.md`:

```raku
class C { has $.v is rw }
class Sink { method take($y is rw) { $y = 9 } }
sub g($y is rw) { $y = 9 }
my $c = C.new(v => 42);

Sink.new.take($c.v);      # raku: 9   mutsu: died, "expects a writable container"
my $r = &g; $r($c.v);     # raku: 9   mutsu: the same refusal
```

Both are closed, along with a third the ticket did not list and four rows that
were **worse than the refusal it claimed**.

## The ticket asked for a judgment call between two routes. Measurement rejected both.

The ticket framed the choice as: a cheap program-wide set-only "any user method
declares a container-binding positional parameter" flag (slice 3a's
`any_raw_invocant_method` pattern), or a principled name-keyed method index
(`Symbol -> bool`, maintained by the same `Registry` mutator that maintains
`owner_method_names`). It called the cheap route dangerous — it would hand a
container to *every* accessor-shaped method argument in any program that
declares one such method anywhere — and said the code-variable row needed the
decision moved into the binder, "a bigger design question than this ticket".

`--dump-bytecode` says neither is necessary, because the premise ("there is no
callee to ask") is wrong. Every one of these spellings pushes its callee
**before** its arguments:

```text
$s.take($c.v)     GetLocal($s);  GetLocal($c); CallMethodMut{"v"}; CallMethod{"take"}
S.new.take($c.v)  GetBareWord(S); CallMethodMut{"new"};  GetLocal($c); ... CallMethod{"take"}
$r($c.v)          GetLocal($r);  GetLocal($c); CallMethodMut{"v"}; CallOnValue
&g($c.v)          (name in the opcode)         GetLocal($c); ... CallOnCodeVar{"g"}
```

The marker is inserted immediately before the *argument's own* trailing
`CallMethod`, so when it executes the callee is already sitting on the stack —
one slot below the argument being evaluated, plus one for each earlier argument
(each leaves exactly one value). The compiler does not know the callee, but the
**VM does**, one instruction before the call.

So the new gate (`OpCode::MarkRwArgRefContextCallee`,
`src/vm/vm_rw_arg_callee.rs`) asks the *actual* callee:

- **a method call** — resolve the receiver's own MRO and ask whether any
  candidate of that name binds a container at that positional index
  (`Registry::any_method_binds_container_at`). Over-approximating across a
  `multi`'s candidates is unavoidable and deliberate — the arguments are still
  being evaluated, so the candidate cannot be selected yet — but it is
  over-approximation *within one `(owner, name)` row*, not across a program;
- **a code value** — read `SubData::param_defs` straight off the stack value.
  Exact, lock-free, no registry involved at all. This is the row the ticket said
  needed a binder redesign;
- **`&g(...)`** — resolve the code variable the way `CallOnCodeVar` resolves it
  and ask the same question, falling back to the by-name registry lookup when
  the routine is declared after its use site.

The cheap flag survives, but demoted to what it is good for:
`Registry::any_container_binding_method_param` is a **pre-filter** in front of
the method branch's MRO walk, never the answer. Corpus measurement is what makes
that distinction concrete: 80 files under `modules/`, `vendor/`, `t/` and
`roast/` declare a container-binding method positional parameter, and 107
contain an accessor-shaped method-call argument — so a program-wide flag used as
the *decision* would not be a rare event, it would fire across most of the
repo's own test corpus.

Every parameter question in the gate reads `ParamDef::binds_caller_container`,
the same predicate the binder uses to decide whether to install the shared cell,
so the producer and the consumer cannot disagree about what "binds a container"
means. And nothing new consumes these containers: the binder's bare-`ContainerRef`
arm, `assign_lvalue_container` and slice 3a's
`try_raw_invocant_container_lvalue` take them unchanged. ADR-0067's part 4 now
stands at six producers and the same consumers.

## The ticket's "both refuse loudly, neither is a silent wrong answer" was true of one parameter flavour out of three

The ticket tested only `$y is rw`, which refuses at the binder. The other two
container-binding spellings do not refuse there, and the survey found a **silent
wrong answer** just outside the ticket's boundary:

| # | Program (`class C { has $.v is rw }`) | raku | mutsu (before) |
|---|---|---|---|
| m01 | `Sink.new.take($c.v)`, `method take($y is rw)` | `9` | dies, "expects a writable container" |
| m02 | the same with `method take(\y)` | `9` | dies, `Cannot modify an immutable Int (42)` |
| m03 | the same with `method take($y is raw)` | `9` | dies, `Cannot assign to a readonly variable` |
| m14 | `method !take($y is rw)` reached via `method go($z is raw) { self!take($z) }` | `9` | **`42` — silent, exit 0** |
| v05 | `sub g(\y) { y = 9 }; my $r = &g; $r($c.v)` | `9` | dies, `Cannot modify an immutable Int` |
| s07 | `&g($c.v)` for `sub g($y is rw)` | `9` | dies, "expects a writable container" |
| k07 | `Sink.new.take($c.v).VAR.^name` | `Int` | dies |

m14 is the shape the rule "an `is raw` parameter accepts anything" produces: the
value copy is accepted, the write lands on the copy, and the program continues.
All seven are green now, along with the inherited, role-composed, `submethod`,
`multi`, `where`-constrained, type-object-invocant, quoted-method-name,
`augment class Int`, `self.`-called and two-frame-relay spellings, and code
values read out of an array element, a hash element, a plain parameter and a
`&`-sigil named parameter.

## What still refuses, all measured, all loudly

- **A subscript argument** (`Sink.new.take(@a[0])`, `$r(@a[0])`) — raku `[9 2]`,
  mutsu refuses. The named-sub twin `g(@a[0])` works, so this is a *different*
  producer (the subscript's element cell, not the accessor's), the argument twin
  of `todo/tickets/subscript-receiver-raw-invocant-producer.md`. Recorded as
  `todo/tickets/subscript-argument-container-producer.md`.
- **A `|@slip` before the accessor** — the marker declines by construction,
  because a slip spreads an unknown number of positionals and the callee's
  parameter index is no longer a compile-time fact.
- **`$obj.^lookup('m')($obj, $c.v)`** — a `Method` object invoked as a code
  value counts its invocant as positional 0, which the `SubData::param_defs`
  read does not model. Recorded in the same ticket.
- **`handles <take>` delegation** — the delegating shim re-dispatches with a
  value. A pre-existing delegation gap, not an argument-producer one.
- **`method take(:$y is rw)`** — raku refuses this *at compile time* ("Cannot
  use 'is rw' on optional parameter"); mutsu accepts the declaration and refuses
  at the call. A parser validation gap, recorded in the same ticket.

## Cost

The new marker is emitted only for an argument-less, unmodified, unquoted
method-call argument, so it does not appear in bytecode a program did not ask
for: dumping all 23 files under `benchmarks/` finds zero occurrences of either
`MarkRwArgRefContext` op. The method branch's MRO walk sits behind a set-only
registration flag, so a program declaring no container-binding method parameter
pays one bool read. No A/B is claimed and none was run: on this box a
byte-identical control drifted +6.3% during ADR-0067's E6 measurement, so
anything under ~7% would be unreadable, and what was measured here is the
marker count, not a time.

Pinned by `t/rw-arg-nameless-callee.t` (38 tests, byte-identical output under
`mutsu` and `raku`).
