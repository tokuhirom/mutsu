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

## Reproduce

The two one-liners above, no fixtures.
