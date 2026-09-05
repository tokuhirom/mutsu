# A renderer FETCHes a `Proxy` held inside the container it renders

ADR-0040 §9 settled what happens when a `Proxy` *enters* a container (it FETCHes
at the store), and §9.1 what happens when it is *bound* into one (the container
keeps it). What was left was what happens when a container holding one is
**rendered**, and mutsu printed the container:

```raku
my $n = 5;
my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
my $l = (1, $p, 3);
say $l;          # raku: (1 5 3)      mutsu was: (1 Proxy 3)
```

A `List`'s elements are not containers, so §9's store FETCH deliberately does not
apply to them — the list keeps the `Proxy`, live. §9.1's `@a[0] := $p` puts one
into an `Array` or `Hash` the same way. Both shapes rendered as `Proxy`, in
**every** renderer: `say`, `.gist`, `.raku`, `.join`, `"$l"` and `~$l` all agreed
with the wrong answer, and so did `@a`/`%h` after an element bind.

## The rule

Recorded as **ADR-0040 §9.2**: *a renderer resolves its receiver's `Proxy`
elements before rendering.*

This is Rakudo's ordinary decont made explicit rather than a new rule. Rakudo
renders a container by calling `.gist`/`.Str`/`.raku` **on each element**, and a
method call deconts its invocant — so a `Proxy` element renders as its FETCHed
value, at render time, with the container still holding the Proxy afterwards.
mutsu's renderers are pure `Value` walkers that format elements inline, with no
per-element dispatch to do that deconting, so the FETCH is hoisted to the
renderer's own entry point, where the interpreter is still in hand; the resolved
value then goes to the same pure renderer, unchanged.

Nor is the mechanism new. `runtime/list_element_stringify.rs` already did exactly
this for an `Instance` element with a user-defined `.Str` — "resolve the elements
in place, then hand the list to the same pure renderer". A `Proxy` element
resolves by FETCH instead of by dispatching `.Str`, and the two compose: a Proxy
handing back an `Instance` still gets that class's `.Str`.

`Interpreter::renders_receiver_elements` names the renderers: `gist`, `Str`,
`Stringy`, `raku`, `perl`, `join`, `fmt`, `say`, `put`, `note`. That is a closed
enumeration of mutsu's *own natives* — the ones that inline a per-element method
call instead of dispatching one — not a heuristic about user code, which is what
makes it acceptable where ADR-0021/0054 rejected a callee-name list. A method
that hands an element to **user** code rather than rendering it — `map`, `grep`,
`sort`, `for` — is deliberately absent: it binds the element container, `Proxy`
included (ADR-0045), and resolving would destroy the very thing it exists to pass
along.

## Where the hooks are

Five sites, each already the point where its spelling's receiver is decided.
mutsu has several dispatch entries, so a receiver decision has to be made at each
— the same duplication `delegates_to_array_storage` carries, and documented at
every site:

| Site | Spelling it catches |
| --- | --- |
| `vm_data_io_ops.rs` (`say`/`put`/`print`/`note`) | `say $l` — these already FETCHed a *top-level* Proxy; the change is depth |
| `vm_call_method_ops.rs` | `(1, $p, 3).gist` — a literal receiver |
| `vm_call_method_mut_ops.rs` | `$l.gist`, `@a.raku` — a *variable* receiver compiles to `CallMethodMut` |
| `methods_call_dispatch.rs` | `$l."$m"()`, and every interpreter-internal render |
| `builtins/functions/flat.rs` (`join_needs_interpreter`) | `"@a[]"`, which compiles to `join(" ", @a)` and reaches no method dispatch at all |

The `~`/`eq` operand path needed no new site: `coerce_stringy_operand` already
runs the `list_str_needs_interpreter` scan for the `Instance` case, so teaching
that scan about `Proxy` costs no second traversal. Each hook tests the method
name first — a `matches!` over a `&str` — and only then scans, so the
allocation-free `value_has_proxy` walk runs only for a call about to visit every
element anyway.

`value_has_proxy` / `resolve_proxies_in_value` also learned to look through a
`ContainerRef`/`ContainerView` to reach a Proxy bound behind an element cell,
which is §9.1's spelling and was otherwise invisible to the scan. The cell is
dropped from the *resolved copy* — what a value-context read does anyway — and
only ever when there is a Proxy inside.

## What deliberately did not change

The container keeps its `Proxy`. `$l[1] = 7` still reaches the `STORE`, `$l`
still re-FETCHes after the backing lexical moves, and `@a[0].VAR.^name` is still
`Proxy`: resolving builds a fresh value for the renderer and never writes back.
FETCHing at *construction* time is the obvious cheap fix and is wrong — the
ticket records the rakudo oracle proving the Proxy stays live in the list.

Pinned by `t/proxy-renders-through-fetch.t` (26 rows), which also pins the
Proxy-free path rendering byte-for-byte as before, itemization included.

Closes `todo/tickets/list-element-proxy-not-rendered-through-fetch.md`.
