# A `Proxy` held in a `List` renders as `Proxy` instead of FETCHing

## The divergence

A `List`'s elements are **not** containers, so — unlike an `Array` element, which
`news/2026-09/proxy-fetches-at-the-container-store.md` made FETCH at the store — a `Proxy` put in a
`List` literal stays a `Proxy` and must FETCH on every *read*, live:

```
$ raku  -e 'my $n=5; my $l = (1, Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v }), 3); $n=9; say $l'
(1 9 3)
$ mutsu -e '...same...'
(1 Proxy 3)
```

Interpolation is wrong the same way (`say "$l"`: raku `1 5 3`, mutsu `1 Proxy 3`).

Note the two oracles differ on purpose in that pair: `say $l` after `$n=9` gives `9` because the
FETCH runs at render time, while `"$l"` built before the mutation gives `5`. Both are FETCHes; only
mutsu prints the container.

## Root cause

`say`/`print`/`note` FETCH only a **top-level** `Proxy` argument
(`auto_fetch_proxy` in `src/vm/vm_data_io_ops.rs:162,210,227`); nothing FETCHes a `Proxy` sitting
*inside* the rendered container. The deep helper already exists —
`Interpreter::resolve_proxies_in_value` (`src/runtime/builtins_lvalue.rs`), written for `eqv` — so
the `say` half is close to a one-line change.

## Why it is a ticket rather than a one-liner

Fixing only `say`/`print`/`note` leaves every other renderer wrong: string interpolation, `.gist`,
`.raku`, `.Str`, `~` concatenation. Those go through `Value::to_display_string` and friends, which
are **pure `Value` methods with no `&mut Interpreter`**, and a FETCH is a call into user code. So
the honest fix needs a decision about where the renderer gets its interpreter from (thread a
context through the display path, or FETCH deeply at every VM-level boundary that hands a value to
a renderer). That is the same shape of question ADR-0040 answered for itemization, and it should
probably be answered as an amendment to it.

Also weigh the cost: `resolve_proxies_in_value` starts with a full recursive `value_has_proxy` scan,
so a naive deep FETCH on every render adds an O(n) traversal to `say @big-array`.

## Update 2026-09-04: the top-level half is done, the nested half is what is left

`news/2026-09/string-context-fetches-a-top-level-proxy.md`: infix `~`, the
string comparators, and interpolation now FETCH a **top-level** `Proxy`
operand (`"x$p"` is `x5`, not `xProxy`), at the two existing chokepoints
`coerce_stringy_operand` and the `StringConcat` loop. That was a divergence in
its own right and was never this ticket's subject.

This ticket's subject is unchanged and still reproduces: a `Proxy` *inside* a
rendered container. `say (1, $p, 3)` still prints `(1 Proxy 3)`. The design
question below is exactly as stated — the remaining renderers are pure `Value`
methods with no `&mut Interpreter` — and is the whole of the remaining work.

## Reproduce

The two one-liners above, no fixtures.

## Re-measured 2026-09-04, and one candidate fix is ruled out

Six renderers diverge, not one, and they are all of them — the value reads are
already right:

| | rakudo | mutsu |
| --- | --- | --- |
| `say $l` | `(1 9 3)` | `(1 Proxy 3)` |
| `$l.gist` | `(1 9 3)` | `(1 Proxy 3)` |
| `$l.raku` | `$(1, 9, 3)` | `$(1, Proxy, 3)` |
| `$l.join(",")` | `1,9,3` | `1,Proxy,3` |
| `"$l"` | `1 9 3` | `1 Proxy 3` |
| `~$l` | `1 9 3` | `1 Proxy 3` |
| `$l[1]` | `9` | `9` ✓ |
| `$l[1] = 7` | stores | stores ✓ |
| `my @a = 1, $p, 3; say @a` | `[1 9 3]` | `[1 9 3]` ✓ (the ARRAY store FETCHes, per ADR-0040 §9) |

**Ruled out: FETCHing when the List is constructed.** That is the obvious cheap
fix — make a list literal behave like the array store — and it is wrong.
rakudo's List keeps the Proxy LIVE:

```raku
my $v = 9; my $p := Proxy.new(FETCH => -> $ { $v }, STORE => -> $, $x { $v = $x });
my $l = (1, $p, 3);
say $l;          # (1 9 3)
$v = 42;
say $l;          # (1 42 3)   <-- re-FETCHed, so the Proxy is still in the list
```

mutsu already agrees about what the list HOLDS (its `$l[1] = 7` reaches the
Proxy's STORE). So the FETCH has to happen at every render, which is exactly the
question the ticket says belongs in an ADR-0040 amendment.

## What the amendment has to decide

`Interpreter::resolve_proxies_in_value` already exists and is already cheap on
the no-Proxy path (`value_has_proxy` is an allocation-free scan) — `is-deeply`
uses it. The open question is *which* sites call it, and there are two shapes:

1. **The statement-level renderers** (`say`/`print`/`put`/`note`, string
   interpolation, `~`) are unambiguous VM chokepoints with `&mut Interpreter` in
   hand. Cheap and obviously correct.
2. **The method renderers** (`.gist`, `.raku`, `.Str`, `.perl`, `.join`, `.fmt`,
   …) are the open-ended half: they reach `Value` methods with no interpreter.
   The natural chokepoint is a "decide the receiver first" guard at
   `call_method_with_values_inner` (the same shape
   `delegates_to_array_storage` uses, see
   `news/2026-09/array-subclass-delegation-is-one-decision.md`) — but that needs
   a *stated rule* for which method names are renderers, which is precisely what
   an ADR should fix rather than a patch.

Doing only (1) is worse than doing neither: `say $l` and `$l.gist` would then
disagree with each other, where today they are at least uniformly wrong.
