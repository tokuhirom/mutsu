# Immutable lvalues mutsu still lets you assign to (survey)

Found by the exception-taxonomy survey in
`news/2026-08/readonly-assign-exception-taxonomy.md`. That work fixed *which*
exception a rejected assignment throws; this ticket collects the cases where
mutsu does not reject the assignment at all, which the same survey surfaced.
Every row was probed against `raku` v2026.06.

## Deep-triage note (2026-09-01)

Moved from `todo/tickets/` because the remaining rows do not share a bounded
implementation surface. Immutable List/Seq element stores and `:=` bindings
need the live element-container semantics owned by ADR-0036 (whose producer
and compensator work remains open), while implicit closure topics additionally
need their direct-call and native map/grep writeback paths separated. Keep this
survey here until those prerequisites make a focused slice independently
verifiable.

## Status update (2026-08-27)

Seven more rows are now **fixed** (`news/2026-08/closure-call-and-bareword-term-readonly-gaps.md`):
a `$`-sigiled single-param pointy block (`-> $v { $v = 1 }`); assigning to the
bare `Nil` term, a builtin type object (`Int = 5`), or an enum value (`enum Fo
<A B>; A = 3`); a sub-signature destructure leaf, both sigilless (`\a`) and
`$`-sigiled (`$x`); `push`/`append`/`unshift`/`prepend`/`pop`/`shift` on an
`@`-var bound to an immutable List (`my @a := (1,2,3); @a.push(4)`); and
postfix/prefix `++`/`--` on a sigilless bind (`my \G = 5; G++`).

The remaining probe harness (unchanged rows only):

```raku
sub p($l, &c) { my $r = try { c() }; say $l, " => ", ($! ?? $!.^name ~ " | " ~ $!.Str !! "OK") }

p "list elem",        { (1,2,3)[0] = 9 };            # raku: X::Assignment::RO, Cannot modify an immutable List ((1 2 3))
p "range elem",       { (1..3)[0] = 9 };             # raku: X::Assignment::RO, Cannot modify an immutable Range (1..3)
p "Seq elem",         { my $s = (1,2,3).Seq; $s[0] = 5 };   # raku: X::Assignment::RO, immutable Int (1)
p "map literal topic",{ (1,2).map({ $_ = 5 }).eager };          # raku: X::AdHoc
p "grep topic",       { (1,2).grep({ $_ = 5 }).eager };         # raku: X::AdHoc
p "block arg topic",  { my $s = { $_ = 5 }; $s(7) };            # raku: X::AdHoc
p "bind list assign", { my $x := (1,2,3); $x = 5 };  # raku: X::AdHoc, Cannot assign to an immutable value
```

(The four topic rows fixed 2026-08-26 -- `for 1,2`, `for (1,2)`, `for <a b>`,
`for %h.keys` -- and the seven fixed 2026-08-27 above are no longer listed.)

## Why `-> $v { $v = 1 }` was fixable but `{ $_ = 5 }` (block-argument topic) is not

Both looked like the same gap ("named routines mark their params, the
closure-call path does not"), but they turned out to need genuinely different
mechanisms, and only one is safe to ship:

- **A `$`-sigiled pointy-block parameter is *always* readonly**, regardless of
  what it is called with (row 1 of the readonly-assign-exception-taxonomy: a
  readonly binding with a container, `X::AdHoc` "Cannot assign to a readonly
  variable or a value") — the same rule a `for`-loop's named alias already
  gets. This does not depend on whether the argument came from a container, so
  it is safe to mark unconditionally.
- **A bare `{ $_ = 5 }` block's IMPLICIT topic is writable exactly when the
  argument is a container** (row 2 vs. row 3 of the same taxonomy). `$s(7)`
  passes a literal `7` — never a container — so it should always be readonly.
  But the exact same VM code path (`call_compiled_closure_with_topic`'s
  "implicit `$_` for bare blocks" branch, `vm_closure_dispatch.rs`) is also
  what `@a.map({ $_ *= 10 })` uses when the native `.map` loop's rw-writeback
  detection (`vm_native_map.rs`'s `mutates_topic`/`writeback_name`) hands it a
  *named array's real element* with `capture_rw_topic: true` — and THAT case
  legitimately mutates the source array (verified: `my @a = 1,2,3;
  @a.map({ $_ *= 10 }); say @a;` prints `[10 20 30]` in both raku and mutsu,
  and this is load-bearing existing behaviour, not a bug). Marking the
  implicit topic readonly at that shared call site would break
  `@a.map({ $_ = ... })`'s rw writeback outright — it is the exact same
  ADR-0040 "does mutsu know the source item is a container" gap the `for`-loop
  topic rows are blocked on, just reached from `.map`/direct-call instead of
  `for`. **Left unfixed; do not attempt without ADR-0040's store-side element
  itemization**, and do not reuse `call_compiled_closure_with_topic`'s
  implicit-topic branch for this without first separating the "direct value
  call" and "native map rw-writeback" callers, which currently share it.
- The two `.map`/`.grep`-literal-topic rows are the same blocker by the same
  mechanism (both go through the identical "implicit topic from a positional
  arg, `capture_rw_topic` may or may not be set" code, since `(1,2).map({ $_ =
  5 })` over non-Pair literal elements takes the exact same `explicit_topic:
  None` path as `@a.map({ $_ *= 10 })` over a real array — the two are
  indistinguishable at that call site without per-item container information).

## A live trap this ticket's fix ran into: bytecode-injected readonly marks leak through "fast native loop" call paths

The first implementation of the `-> $v { $v = 1 }` fix injected a
`Stmt::MarkReadonly` prologue statement into the pointy block's *compiled
body*, mirroring how `Stmt::MarkSigillessReadonly` already does this for `my
\x = 5`. That reached CI green locally but broke `t/digest-battery.t`,
`t/map-native-rw-param.t`, and `t/topic-alias-does-not-cross-frames.t`:
`resolution_map_grep.rs`/`resolution_map_grep_rw.rs` (the native `.map`/`.grep`
loops, plus a `.first` matcher) bind a block's params by a direct `env.insert`
and run its body via `run_reuse` *without* `push_call_frame`/
`enter_readonly_frame` (a deliberate perf shortcut around the general call
machinery). A prologue statement runs unconditionally wherever the bytecode
executes, so it marked `readonly_vars` with `readonly_frames == 0` — which
skips the undo journal entirely (`mark_readonly_sym_with`'s `if
self.readonly_frames.get() == 0 { return; }`) and leaked the mark
PERMANENTLY into the next unrelated same-named lexical anywhere later in the
program (SHA3's `map -> $x {...}` leaking "x" into a later `for ... -> ($x,
$y)` reusing the name; digest's rw-map "x" leaking across an outer `for`
iteration).

**Fix actually shipped:** mark the parameter at the CALL SITE
(`call_compiled_closure_with_topic` in `vm_closure_dispatch.rs`, gated on a
new `CompiledCode::pointy_alias_param` flag set only for this one shape) right
after `bind_function_args_values` succeeds, not via a body prologue.
`push_call_frame` already ran earlier in that same function, so the mark is
correctly scoped and rolled back on every exit. This also means the mark is
simply never applied when a native fast loop bypasses that function entirely
— which is exactly the "leave the topic rows alone" boundary this ticket
already draws, achieved as a side effect rather than a special case.

The two leaky loops (`eval_map_over_items` and `eval_map_over_items_rw`) were
also hardened directly with a `ReadonlyFrameGuard` around each iteration's
`run_reuse` call, so a *future* body side effect of the same shape doesn't
reintroduce this class of leak.

## A second live trap: the bareword-term check can't just test the CURRENT value

The `Nil`/type-object/enum fix (added to the `SetGlobal` opcode handler)
rejects an assignment when the target's CURRENT env value looks like a type
object, `Nil`, or an enum member. That is unsound on its own: a bareword
carries no sigil in its *storage key* either way (`Nil = 5` and `$_ = 5` both
compile to `Stmt::Assign { name: "_"|"Nil", ... }` — the sigil is stripped
before this point, same information loss `constant $PI` vs. `constant PI` hit
in the exception-taxonomy work), so a check keyed only on "does the current
value look like an enum" cannot tell the true case (`enum Fo <A B>; A = 3`)
apart from a plain variable that merely happens to currently hold a COPY of
that same enum value — e.g. `$_ = $state` where `$state` holds `A` from an
earlier loop iteration reached `t/topic-alias-does-not-cross-frames.t` and
broke it (mutsu called `.=` on the container-less topic and reported "Cannot
modify an immutable State (A)").

The reason only `$_` triggers this SPECIFIC ambiguity in practice: a simple
`my`/`our`-declared variable (`my $state = ...`) compiles to a local SLOT
(`SetLocal`, never reaching this `SetGlobal`-only check at all), so it can't
collide with a bareword this way. `$_` is dynamically resolved via env by
design on every frame, never slot-allocated — and a bareword `_` alone is
never a valid Raku term regardless (rejected at `exec_get_bare_word_op`'s very
first check). So excluding `name == "_"` from this check is a principled fix
for that specific case, not a narrow patch papering over the real gap —
verified against `my $state`/`our $state` holding-then-reassigning an enum
value, neither of which reaches this code path at all. (This claim held for
the ENUM check; the type-object check needed a further refinement — see the
third trap below, which found that NOT EVERY variable avoids `SetGlobal` the
way a simple `my $x` does.)

## A third live trap: a lowercase bareword's storage key ALSO collides with a variable, on its first-ever write

The type-object check's `unbound_type_slot` test originally treated "never
referenced at all" (`env.get(name)` is `None`) the same as a pre-seeded `Nil`
slot — necessary for `Int = 5` to be caught, since a builtin type's bareword
is never actually written into env by anything else. That broke
`roast/S32-str/comb.t`: `for @tests -> ($str, $expected, |args) {...}` binds
its sub-signature destructure leaves by a direct `SetGlobal`, NOT a local
slot (destructure leaves aren't slot-allocated the way a simple `my $str` is —
contradicting the "an ordinary variable always compiles to SetLocal" claim
above, which is true for a simple declaration but not for every binding
shape). `$str`'s very first write hit the exact same "never referenced" check
and was misidentified as assigning to the lowercase native type `str`.

**Fix:** restrict the `None`-counts-as-unbound inference to TitleCase names
(`name.starts_with` an uppercase char). `str`/`int`/`num`/`array`/`bool`/...
are realistic, common variable names whose sigil-stripped key collides with a
type synonym; `str = 5` as a bare statement referencing the TYPE object is not
idiomatic Raku at all. So the lost coverage (a lowercase native-type
bareword's very first, never-yet-referenced assignment) is a deliberate,
accepted trade-off — `Int`/`Nil`/a user class (TitleCase by convention) still
work, and a genuinely ambiguous case would need the sigil-vs-bareword
distinction preserved at compile time (the same class of fix the
exception-taxonomy work made for `constant $PI` vs. `constant PI` via the
parser-recorded `__constant_sigil` trait) to close soundly.

**This same ambiguity turned out to have a THIRD current-value shape, not just
`None`/`Nil`: `Package(Any)`.** That one reached CI as `roast/S02-types/set.t`
and `sethash.t` going red with an unrelated-looking symptom — `lives-ok`/
`dies-ok` false negatives (`my $str; lives-ok { $str = 1 }, "x"` reported "not
ok" even though the assignment succeeded and nothing threw). Root cause:
`SetVarDynamic`'s closure-capture-by-reference support pre-seeds ANY not-yet-
assigned closure-captured variable's env slot with the placeholder
`Package(Any)`, regardless of the variable's own name (the same mechanism
`exec_get_bare_word_op`'s read-side fallback already special-cases). The
`Package(_)` branch of the original fix was left completely unconditional —
no TitleCase gate at all — so a captured, not-yet-initialized `$str` hit this
placeholder on its first (`lives-ok`-internal) write and was misidentified as
assigning to the type `str`; `lives-ok` correctly caught the resulting
spurious `X::Assignment::RO`, which is why the symptom looked like a `Test`
bug rather than an assignment bug. Fixed the same way: `Package(Any)` (for any
name other than the literal bareword `Any`) now requires the same TitleCase
gate as `None`/a real `Nil`; only a genuine `Package(SomeRealType)` (set
exclusively by actual class/type registration) is trusted unconditionally.
`t/immutable-lvalue-assignment-gaps.t` pins all three shapes as separate
regression controls (a `for`-loop destructure leaf, a `lives-ok`-captured
uninitialized variable, and the direct `Int`/`Nil`/`Foo` cases) so a future
change to this check has to keep all three honest at once.

## Still blocked: the topic rows (`for`-loop per-item container-ness)

`for %h`, `for @a[0..1]`, `for @a.map(…)` remain blocked on ADR-0040's
store-side element itemization, unrelated to the mechanism above:

```
for @a         Scalar      for 1,2        Int
for @a.values  Scalar      for (1,2)      Int
for $a, $b     Scalar      for <a b>      Str
for @a[0..1]   Scalar      for %h.keys    Str
for @a.map({}) Scalar      for %h         Pair
```

mutsu cannot evaluate that at runtime because real `Array`/`Hash` elements are
stored **bare** — see `todo/deep/element-itemization-lost-in-scalar-binding.md`
and ADR-0040. `vm_for_loop_lazy.rs` already applies the correct runtime test
(`item.is_container_ref()`), which is why `for gather { … }` is rejected
correctly; applying the same test on the eager path would additionally mark
`for @a[0..1]` and `for @a.map(…)` read-only, inventing throws raku does not
have. **These rows are therefore blocked on ADR-0040's store-side element
itemization, not on the topic-marking code.**

## Still blocked: `my $x := (1,2,3); $x = 5`

The deliberate conservatism in `src/vm/vm_var_assign_set_local.rs`: the
`:=`-to-literal readonly marking fires only for scalar-shaped values
(`Int`/`Str`/`Num`/`Rat`/`Bool`/`Complex`), because anything container-like or
written-through (`ContainerRef`, `Proxy`, `HashEntryRef`, `is raw`) must stay
writable and an overlooked kind becomes a hard error. Widening it needs the
container/no-container distinction to be a property of the *value*, not a
whitelist of view kinds.

## Still blocked: element assignment (`(1,2,3)[0] = 9`, `Range`, `Seq`)

Need the subscript store path to know its target is an immutable container —
the same underlying container/no-container information ADR-0040 would supply.

## Status update (2026-08-27): ADR-0040 slices 1-2 landed and did NOT unblock these rows

The 2026-08-26 note above says the `for` topic rows are "blocked on ADR-0040's store-side
element itemization". ADR-0040 slices 1 and 2 have both landed (mutation sites and
construction sites — a real `Array`/`Hash` element that holds an aggregate is now itemized),
and every row was re-measured against it. **None moved.** `(1,2,3)[0] = 9`,
`(1..3)[0] = 9`, the `Seq` element, `map`/`grep` topics and `for %h` all still succeed
silently in mutsu, and `for @a[0..1] { $_ = 5 }` still fails to write through.

The re-measurement corrects the blocker attribution, which is the useful outcome:
**itemization is not container-ness.** ADR-0040 makes an element render as one item
(`ArrayKind::ItemArray` / a `Scalar` wrapper); it does not make the element a *cell*, and
`item.is_container_ref()` — the runtime test the note proposes reusing on the eager `for`
path — is still False for an ordinary itemized element. Worse, itemization is orthogonal to
what these rows need: a plain `Int` element of a real `Array` is writable and is *not*
itemized (§2's negatives), while a `List` literal's `List` element is not writable and, in a
`for` over a real array, *is*. So the flag cannot be used as the writability oracle even in
principle.

The property these rows actually need is ADR-0036's surface — promoting the element that is
handed out to a real `ContainerRef` cell — plus, for `map`/`grep`/pointy-block topics, the
missing readonly marking on the closure-call binding path (which ADR-0040 never touched and
which is independent of both). Re-point the blocker at
[ADR-0036](../../docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md) rather
than at ADR-0040.

## Messages that are close but not exact

These already throw the right class; only the rendered value differs:

- `my constant @A = 1,2,3; @A = 5` — raku names the *element*
  ("Cannot modify an immutable Int (1)", because a list assignment writes into
  the immutable List's elements); mutsu names the container
  ("Cannot modify an immutable List (1 2 3)"). Same for `my @a is List`.
- `my constant %C = (a=>1); %C = (b=>2)` — raku "Cannot modify an immutable Pair
  (a => 1)"; mutsu renders the pair with a tab instead of `=>`.
- `my %m := mix <a b>; %m = (c=>1)` — raku "immutable Mix (Mix(a b))", mutsu
  "immutable Mix (a b)".
- `sub g() {...}; g() = 5` — raku "Cannot modify an immutable Int (42)", mutsu
  "sub 'g' is not rw"; `$obj.x = 5` on a non-`rw` attribute — raku
  "Cannot modify an immutable Int (1)", mutsu "method 'x' is not rw".
- `my @a := (1,2,3); @a.splice(0,1)` — raku does not even define a `splice`
  candidate on a plain `List` (`X::Multi::NoMatch`, "Routine does not have any
  candidates"); mutsu reports `X::Immutable` "Cannot call 'splice' on an
  immutable 'List'" (the same message the other five list mutators correctly
  use, since `splice` shares their dispatch check). Pre-existing, not
  introduced by the 2026-08-27 push/append/unshift/prepend/pop/shift fix above
  (which only extended that same check from `$`-bound to `@`-bound targets).

## Re-verified 2026-09-01 (TRIAGE regeneration)

Six of the seven remaining harness rows still answer `OK` (list elem, Seq
elem, map literal topic, grep topic, block arg topic, bind list assign; plus
`for %h { $_ = 5 }`). One row moved: `(1..3)[0] = 9` now throws
`X::Assignment::RO` in mutsu — the right class, but rendered as `Cannot modify
an immutable value (1 2 3)` where raku says `... immutable Range (1..3)`, so it
belongs in the "close but not exact" section rather than the live harness.
Blocker attribution (ADR-0036, not ADR-0040) unchanged.
