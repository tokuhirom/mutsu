# Seven more immutable-lvalue assignment gaps closed

`todo/tickets/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md` (the
survey the exception-taxonomy work spun off) listed cases where Raku rejects
an assignment to an immutable lvalue but mutsu silently accepted it. Seven of
those rows are now fixed, each verified against `raku` v2026.06 for both
exception class and message:

- **A `$`-sigiled pointy-block parameter is now readonly**: `my $b = -> $v {
  $v = 1 }; $b(3)` now throws `X::AdHoc` "Cannot assign to a readonly variable
  or a value", matching a non-`is rw` sub parameter and a multi-param pointy
  block (both already correct). A single-param, trait-less pointy block
  (`-> $v {...}`) compiles via `Expr::Lambda` with an empty `param_defs`
  (`bind_function_args_values`'s "legacy" binding path), so it never reached
  the general "mark parameters readonly unless `is rw`/`is copy`/`is raw`"
  logic a real `ParamDef` goes through.
- **Assigning to a bareword that names an immutable VALUE**: `Nil = 5`
  (`X::Assignment::RO`, "Cannot modify an immutable Nil value"), `Int = 5`
  (same class, "Cannot modify an immutable 'Int' type object"), and an enum
  value (`enum Fo <A B>; A = 3`, "Cannot modify an immutable Fo (A)") all now
  throw. These are one mechanism, not three special cases — the existing
  `SetGlobal` guard against reassigning a type object was scoped to
  `self.has_class` (user-declared classes only) and, for the class case it
  did catch, threw an untyped `X::AdHoc` instead of `X::Assignment::RO`; both
  bugs are fixed together.
- **Sub-signature destructure leaves are now readonly**: neither a sigilless
  leaf (`sub f($ (\a, \b)) { a = 1 }`) nor a `$`-sigiled leaf (`sub f($ ($x,
  $y)) { $x = 1 }`) was ever marked — `bind_sub_signature_from_value` is a
  distinct, purely-runtime binding path that neither the parser's
  `MarkSigillessReadonly` prologue nor the ordinary flat-signature marking
  branch reaches.
- **`push`/`append`/`unshift`/`prepend`/`pop`/`shift` on an `@`-var bound to an
  immutable List** (`my @a := (1,2,3); @a.push(4)`): the scalar twin
  (`my $a := (1,2,3); $a.push(4)`) already correctly threw `X::Immutable`, but
  the check (`methods_mut_dispatch.rs`) excluded any `@`-sigiled target, and a
  SEPARATE fast-path opcode (`OpCode::ArrayPush`, `vm_data_push_ops.rs`)
  accepted any `ValueView::Array` kind at all rather than checking
  `kind.is_real_array()`, so it silently mutated the supposedly-immutable
  List in place.
- **`my \G = 5; G++`/`G--`/`++G` now throw `X::Multi::NoMatch`**: a sigilless
  bind's readonly-ness lives in a separate `__mutsu_sigilless_readonly::NAME`
  env-key mechanism from `readonly_vars`; plain assignment already consulted
  it, in/decrement did not.

## Two traps this fix ran into (both worth remembering)

**A prologue statement injected into compiled bytecode runs on every path
that executes that bytecode, not just "the call".** The first cut of the
pointy-block-parameter fix injected a `Stmt::MarkReadonly` prologue into the
block's compiled body (mirroring `Stmt::MarkSigillessReadonly`). That passed
locally but broke `t/digest-battery.t` and `t/map-native-rw-param.t` in CI:
`resolution_map_grep.rs`/`resolution_map_grep_rw.rs` bind a block's params by
direct `env.insert` and run the body via `run_reuse` — a deliberate
`push_call_frame`-bypassing perf shortcut for `.map`/`.grep`/`.first`. A mark
made with no readonly frame open skips the undo journal
(`mark_readonly_sym_with`'s `readonly_frames == 0` early return) and leaks
PERMANENTLY into the next unrelated same-named lexical anywhere later in the
program. The fix moved the mark to the CALL SITE
(`call_compiled_closure_with_topic`, gated on a new
`CompiledCode::pointy_alias_param` flag) instead of the compiled body, where
`push_call_frame` already opened a properly-scoped frame — and, as a bonus,
this means the mark is simply never applied on the native fast-loop paths,
which is exactly the boundary the ticket's "leave `.map`/`.grep` topics alone"
already drew. The two leaky loops were also hardened with a
`ReadonlyFrameGuard` around each iteration regardless, so a future body side
effect of the same shape can't reintroduce the leak.

**A bareword's storage key carries no sigil, so a check on the CURRENT value
alone can't tell "the term itself" from "a variable that happens to hold a
copy of that value."** The Nil/type-object/enum fix first broke
`t/topic-alias-does-not-cross-frames.t`: `$_ = $state` where `$state` held an
enum value from an earlier loop iteration was rejected as if `$_` were the
enum member itself. The fix excludes `name == "_"` specifically — the topic
is the one variable that is both stored bare (no twigil, unlike `$*foo`/
`$!attr`) and never slot-allocated (unlike an ordinary `my`/`our` variable,
which reaches a different, unaffected code path or keeps its own twigil), and
a bareword `_` alone is never a valid Raku term regardless.

**A third trap, found after the first two: a bareword's sigil-stripped storage
key can ALSO collide with a lowercase native-type synonym on a variable's
first-ever write.** Extending the `unbound_type_slot` inference to treat
"never referenced at all" (`env.get(name)` returns `None`) the same as a
pre-seeded `Nil` slot was necessary for `Int = 5` to be caught at all (a
builtin type's bareword is never actually written into env by anything), but
it also broke `roast/S32-str/comb.t`: `for @tests -> ($str, $expected, |args)
{...}` binds its sub-signature destructure leaves by a direct `SetGlobal`
(not a local slot — for-loop destructure leaves aren't slot-allocated the way
a simple `my $str` is), so `$str`'s very first write reached the exact same
"never referenced" check and was misidentified as assigning to the lowercase
native type `str`. The fix restricts the `None`-counts-as-unbound inference to
TitleCase names only (`name.starts_with` an uppercase char): `str`/`int`/
`num`/`array`/`bool`/... are realistic, common variable names whose
sigil-stripped key collides with a type synonym, while `str = 5`/`int = 5` as
a bare statement referencing the TYPE is not idiomatic Raku at all — so the
coverage lost (a lowercase native-type bareword's very first, never-yet-
referenced assignment) is a deliberate, safe trade-off, and `Int`/`Nil`/a
user class (all TitleCase by convention) keep working.

## What's still open

**`my $s = { $_ = 5 }; $s(7)` (a bare block's implicit topic) is deliberately
left unfixed, NOT missed.** It looks like the sibling of the pointy-block-
parameter fix above — the ticket originally grouped both under "named routines
mark their params, the closure-call path does not" — but they need genuinely
different mechanisms:

- A `$`-sigiled pointy-block PARAMETER is *always* readonly regardless of what
  it is called with (a `for`-loop's named alias gets the same rule), so
  marking it unconditionally at the call site is sound.
- A bare block's implicit TOPIC is readonly only when the argument is not a
  container — `$s(7)` passes a literal `7` (never a container, so it should
  always be rejected), but the *exact same* VM code path
  (`call_compiled_closure_with_topic`'s "implicit `$_` for bare blocks" branch)
  is also what `@a.map({ $_ *= 10 })` uses when the native `.map` loop hands it
  a *named array's real element* with `capture_rw_topic: true` — and that case
  legitimately mutates the source array (verified: `my @a = 1,2,3; @a.map({
  $_ *= 10 }); say @a;` prints `[10 20 30]` in both raku and mutsu — this is
  load-bearing existing behavior, not a bug, and this test's own regression
  suite pins it). Marking the implicit topic readonly at that shared call site
  would break `.map`'s rw writeback outright. This is the exact same "does
  mutsu know the source item is a container" gap the `for`-loop topic rows are
  blocked on (ADR-0040), just reached from `.map`/a direct value call instead
  of `for`. It is intentionally left in
  `todo/tickets/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md`,
  with this reasoning spelled out there too.
- The two `.map`/`.grep`-literal-topic rows (`(1,2).map({ $_ = 5 })`) are the
  same blocker by the same mechanism.

The ticket file lists the remaining rows and why each is blocked: the
`for`-loop per-item topic rows and `.map`/`.grep`/block-argument block-topic
mutation both need ADR-0040's store-side element itemization (mutsu can't yet
tell, at the point a block's topic is bound, whether the source item was a
container); `my $x := (1,2,3); $x = 5` is deliberately conservative pending
the same container/no-container distinction becoming a property of the value
rather than a view-kind whitelist; and element assignment (`(1,2,3)[0] = 9`,
Range, Seq) needs the subscript store path to know its target is immutable.

## Tests

`t/immutable-lvalue-assignment-gaps.t` pins all seven fixes (class and
message) plus negative controls (`is rw`/`is copy` pointy-block params, `for
@a { $_ = ... }` over a real Array, `@a.map({ $_ *= 10 })`'s rw writeback, a
real Array's `push`) — verified to pass verbatim under both `raku` and mutsu.
