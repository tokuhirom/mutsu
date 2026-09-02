# A `:=` alias is a container, not a name

`todo/deep/sigilless-alias-closure-capture-skips-typecheck.md` was filed as a
type-check gap, retitled twice, and routed at various points to ADR-0055
(closure free vars) and to closure-capture cell population. All of those
readings were wrong. The defect was one line in the declaration-bind path, and
the file's own last note — "the remaining suspect is the alias identity itself,
not how a closure's captured env is merged" — was right.

## What was measured

`$t := $s` (a REBIND) and `my $t := $s` (a DECLARATION bind) behaved
completely differently, which is what located the bug:

| shape | before | raku |
|---|---|---|
| `my $t := $s; $t = 42` (same frame) | `42` | `42` |
| `my $t := $s; my $f = { $t = 42 }; $f()` | source unchanged | source written |
| `my $t := $s; my $f = { $t }; $s = "z"; $f()` | stale capture value | live |
| `my \x := $s; sub f { x = 42 }; f()` | source unchanged | source written |
| `my Int $a; my \x := $a; sub { x = "s" }()` | wrote through unchecked | type-checks |
| `my $t; $t := $s; my $f = { $t = 42 }; $f()` | **already correct** | correct |

The last row is the tell. `Interpreter::exec_set_local_op`'s scalar bind branch
promoted the pair to a shared `ContainerRef` cell only for
`source_in_outer_frame || (is_rebind && source_in_same_scope && ...)`. A
same-scope *declaration* bind of a plain scalar matched neither, so it fell
through to `local_bind_pairs` — a slot-pair table that reconciles two slots
**inside the declaring frame only**. Everything downstream then followed:

- a closure captured the alias's dereferenced VALUE, so reads were frozen at
  capture time;
- a write from the closure compiled to `SetGlobal`, whose `ContainerRef`
  write-through found a plain value under the name and stored into the
  closure's own by-name copy — the alias read back as `42` while the source
  stayed `"a"`;
- with no cell there was no cell-carried constraint either, so
  `check_container_cell_constraint` had nothing to check.

## What changed

**The declaration bind takes the same route as the rebind.** The gate now
admits `is_vardecl && source_in_same_scope && val_is_simple_scalar`, excluding
the per-call pseudo-variables (`$_`, `@_`, `%_`, `$!`) for the reason recorded
at `is_percall_pseudo_var`: `my $ex := $_` must capture the topic's current
referent, never promote `_` itself.

**A freshly minted cell inherits the source's `of`-type**, so the constraint
travels with the container rather than with the name — ADR-0042's rule applied
at the promotion site. `CellConstraint` gained an `assign_to` field carrying the
variable the cell was promoted from, so the failure reads "Type check failed in
assignment to `$a`" from any frame. That name was missing from every
cell-constraint failure before, including on the rebind path that already
worked.

**A same-scope redeclaration no longer strands its aliases.** The
per-iteration-freshness clear (which drops a stale `ContainerRef` out of a
redeclared slot so a loop's capture-boxed local cannot be written through)
now keeps the cell when `local_bind_pairs` — the per-call-frame table of `:=`
source→target slot pairs — says another lexical of this same frame is bound to
that slot. That is raku's "a redeclaration is the same variable" rule
(`my $x = 2; my $y := $x; my $x = 3` ⇒ `$y == 3`; raku only warns
"Redeclaration of symbol"). A capture-boxed loop local shares its cell with a
CLOSURE and has no such pair, so the two cases do not overlap. An earlier,
broader spelling of this guard — "any other slot holds the same cell" — was
wrong and showed it: it also matched a legitimately shared caller/parameter
cell and broke `t/scoped-overlay-named.t`.

## The second defect: a sigilless bind read its mutability off the syntax

`build_sigilless_bind_stmt` decided with `matches!(expr, Expr::Var(_))`: bind to
a plain variable and the name aliased its container, bind to anything else and
the name was marked readonly outright. Every other lvalue shape was therefore
refused:

```raku
my @a = 1, 2;
my \x := @a[0];
x = 9;              # was: Cannot modify an immutable Int (1)
say @a;             # raku: [9 2]
```

`%h<k>`, a computed index (`@a[$i + 0]`), and an `is rw` accessor call
(`$c.v`) all failed the same way — while `my \x := $s.uc` genuinely *is*
immutable, and the compiler cannot tell that call from the accessor.

Raku decides from the binding, not from the syntax. mutsu now splits the
question in two. The parser asks only "can this RHS *denote* a container" — a
variable, an element, or a method call (an `is rw` accessor is a method call and
the parser cannot tell it from an ordinary one) — and routes those through the
bind machinery. The new `OpCode::MarkSigillessBind` then settles the actual
mutability immediately after the declaration, by looking at what the name holds:
a `ContainerRef`, `Array`, `Hash` or `Proxy` is writable, anything else is the
value itself. So `my \x := $c.v` aliases the attribute while `my \x := $s.uc`
stays immutable, with no static guess about which method was called.

The op writes the ordinary `__mutsu_sigilless_readonly::<name>` marker the
parser used to set statically, so every existing consumer (the `CheckReadOnly`
gate in any frame, the `++`/`--` mutability check, the redeclaration clear)
keeps working unchanged, and it only ever *raises* that marker — a bind whose
source is itself readonly (`sub f($ro) { my \c := $ro }`) has already set it and
must not be lowered.

A literal or any other plainly-computed rvalue keeps the old readonly path
rather than being routed through the bind machinery. That is not just an
optimisation: giving `my \term = 43` a container cell made it collide with a
callee's later same-named `my $term` (`t/scoped-overlay-named.t`) and dropped
the `__mutsu_constant_var::` marker that preseeds EVAL's term table
(`t/eval-compunit-introspection.t`). Both were caught by the local suite before
this landed.

## A third defect, uncovered by the second

Routing the sigilless element bind through the `:=` machinery immediately turned
`roast/S32-str/val.t` red — 1413 of 1453 assertions — on
`my \allo-type := %type2allo{type}`, which answered `Any`. That was a
**pre-existing** bug in the bind-index path, not a new one: the `$`-sigil
spelling failed identically (`my %h{Any}; %h{Int} = IntStr; my $t := %h{Int}`
was already `Any` before any of this), and nothing in the suite had exercised
it.

An object hash stores `.WHICH`-encoded keys. The read path and the `:p`/`:kv`
path both encode the subscript that way when `key_type.is_some()`; the bind
subscript in `exec_index` used `Value::hash_key_encode` unconditionally, so it
looked up the stringified key, missed, and handed back the missing-key default.
It now encodes like its siblings.

## Pin

`t/bind-alias-is-a-container.t`, 34 assertions: the alias live in both
directions in its own frame, from a stored closure, from an anon sub, from a
named sub, and across two nested closure boundaries; the constraint and its
message; each lvalue RHS shape; each value RHS shape staying immutable; and the
shapes that already worked (rebind, sigilless parameter, shadow safety, topic
bind) kept as controls; plus the object-hash element bind in both spellings.

## Not fixed here

- `for $a, 1_000, $b, 1_000_000 -> \x, $value { x = $value }` still writes
  through to nothing (`todo/deep/for-loop-pointy-sigilless-param-write-through-missing.md`).
  It is a different bind shape that never registers an alias at all.
- `my (\a, \b) := ($x, $y)` still copies
  (`todo/tickets/list-destructuring-sigilless-bind-copies-instead-of-binding.md`),
  but its prerequisite is now gone — see that ticket.
- A named sub closing over a file-scope `\x` makes the binding a mainline
  lexical that leaks into a LATER same-named declaration
  (`todo/deep/mainline-lexical-sigilless-binding-leaks-into-a-later-redeclaration.md`).
