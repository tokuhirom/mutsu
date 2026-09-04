# ADR-0041: A sub's *callability* is hoisted for the whole scope, but its *bareword-reference visibility* at `constant`/`BEGIN` time must follow textual order — these are two different questions the current single hoisted registry conflates

- Status: Proposed (investigation only; no implementation plan chosen)
- Date: 2026-08-20
- Related: ADR-0024 (mainline lexicals for named subs), ADR-0039 (`@`/`%`
  lexicals must resolve lexically — the container-side analog of this same
  "flat global name resolution vs true lexical/textual order" family of bugs)
- Addresses: item 2 of
  `todo/deep/user-postcircumfix-index-not-dispatched-for-instances.md`
  (`&postcircumfix:<[ ]>` as a pre-augmentation callable term)

## 1. Context

### 1.1 The symptom that motivated this ADR

`todo/deep/user-postcircumfix-index-not-dispatched-for-instances.md` item 2
described a narrow-looking gap: `&postcircumfix:<[ ]>` doesn't exist as a
callable term, so `my constant &old-same = &postcircumfix:<[ ]>;` (the
`Array::Rounded` idiom for delegating back to native indexing from inside a
user-added multi candidate) "fails outright". Re-investigating on current
`main` (2026-08-20) shows the actual failure is worse and different: a
**stack overflow**, not a lookup failure:

```raku
class Foo is Array {}

my constant &old-same = &postcircumfix:<[ ]>;

proto sub postcircumfix:<[ ]>($, |) is export {*}
multi sub postcircumfix:<[ ]>(Foo:D \SELF, Int:D $index) {
    old-same SELF, $index
}

my Foo $f = Foo.new(10, 20, 30);
say $f[1];   # raku: 20   mutsu: stack overflow (infinite recursion)
```

`&postcircumfix:<[ ]>` at the `my constant` statement does resolve to
*something* in mutsu — it resolves to the module's **own** `multi sub
postcircumfix:<[ ]>` (declared three lines *later*), so `old-same` calls right
back into the same candidate that calls it, forever.

### 1.2 Root cause: this is not specific to postcircumfix at all

The identical class of bug reproduces with a plain named sub, no operators
and no `Array::Rounded`-style idiom involved — `tmp/const-alias-hoisting2.raku`:

```raku
proto sub foo($) {*}
multi sub foo(Int $x) { $x + 1 }

{
    my constant &old-foo = &foo;
    proto sub foo($) {*}                    # shadow in an inner block
    multi sub foo(Int $x) { old-foo($x) * 10 }
    say foo(5);
}
```

Real `raku`: `60` (the inner `old-foo` alias correctly captures the *outer*
`foo`, i.e. `(5+1)*10`). mutsu: `Redeclaration of routine 'foo'. Did you mean
to declare a multi-sub?` — a **different** failure mode than the
postcircumfix case (a hard compile error instead of silent mis-capture),
because mutsu's redeclaration check happens to be name-based and scope-blind
for plain subs, while operator names (`postcircumfix:<...>`,
`infix:<...>`, ...) are apparently exempted from that same check (a
legitimate exemption in isolation — many modules independently add operator
overload candidates under the same op name — but it is what lets the
postcircumfix case slip past redeclaration and into silent self-recursion
instead).

Tracing the actual mechanism (`src/compiler/helpers_ast_utils.rs`,
`hoist_sub_decls` — called from `compiler/mod.rs:3034` for every compiled
block/mainline): **every** top-level `SubDecl`/`ProtoDecl` in a block is
pre-registered via a `RegisterDecl` opcode emitted *before* any other
statement in that block, regardless of its own textual position. The comment
on `Compiler::hoisted_sub_plans` confirms the mechanism is a true in-place
back-fill, not two independent registrations: "A sub declaration is
registered twice: once from the hoist pass at the top of the enclosing block
... The source-order site therefore back-fills its keys into the matching
hoisted plan (same name + fingerprint)". So by the time the very *first*
statement in a block executes — including a `my constant &alias = &name;`
that textually precedes every `sub`/`multi`/`proto name` in the same block —
every sub declared *anywhere* in that block already has a live entry in
`self.registry().functions`, indistinguishable from one declared earlier.

This hoist exists for a real reason and must NOT simply be removed: Raku
genuinely allows calling a sub from anywhere in its enclosing scope
regardless of textual order (`say foo(); sub foo() { 42 }` is legal and
common). What's wrong is narrower: **`&name` bareword *reference* capture
inside compile-time-evaluated constructs (`constant`, `BEGIN`) must see only
the declarations textually before it, while a plain `name(...)` *call* may
continue to see the whole scope** — real Raku's `constant`/`BEGIN` execute
interleaved with compilation, in source order, so they only ever see what has
compiled *so far*; ordinary calls execute after the whole enclosing scope has
finished compiling (and, for named subs, installing their stubs), so they see
everything. mutsu's `self.registry().functions` is a single flat map with no
way to ask "what did this name resolve to as of textual position N" — it
answers only "what does this name resolve to right now", and hoisting means
"right now" is effectively always "fully resolved", even at the very first
statement of the block.

### 1.3 Why this is the sub-declaration analog of ADR-0039

ADR-0039 (Proposed, unimplemented) already diagnosed the identical *shape* of
bug for `@`/`%` container lexicals: mutsu resolves declarations through a
flat by-name `env`, so an inner-scope `my @a` and an outer/sibling-scope
`my @a` alias each other instead of shadowing. This ADR is the same root
misdiagnosis (name resolution that should be lexical/textual-order-sensitive
is instead flat and global-generation-blind) applied to the **function
registry** rather than the **variable environment** — a structurally
different data structure (`registry().functions: HashMap<Symbol,
Arc<FunctionDef>>` vs `env: HashMap<String, Value>`), so the fix is not a
drop-in reuse of ADR-0039's slot/cell mechanism, but the underlying lesson
("declarations must carry textual-position information visible-only queries
can respect, not just a name") is the same lesson. A reader picking up either
ADR should read the other first.

## 2. What a fix needs to preserve

- **Do not break forward-reference calling.** `sub b() { a() } sub a() { 1 }
  say b();` and its cross-package/`use`-import equivalents must keep working
  exactly as today — this is the entire reason `hoist_sub_decls` exists.
- **`&name` bareword-reference capture at `constant`/`BEGIN` evaluation time
  must see only what is textually declared before it** in the enclosing
  lexical scope (and, transitively, in enclosing outer scopes/CORE) — NOT the
  as-yet-unreached remainder of the same scope.
- Real Raku's actual behavior for `&postcircumfix:<[ ]>` specifically (no
  prior user override anywhere) is presumably the CORE `Sub`/dispatcher for
  the built-in subscript operator — mutsu has no such native `Sub` value
  registered at all today (the ORIGINAL, narrower framing of item 2). Any fix
  needs this native term to exist as the fallback `&name` resolves to when no
  user declaration precedes the reference point, or the alias just captures
  `Nil`/fails a different way.

## 3. Options considered

### Option A — a separate "declared-so-far" side table, consulted only by bareword-reference resolution inside compile-time constructs

Keep `hoist_sub_decls`'s existing pre-registration (preserves forward-call
semantics unchanged) but add a second, lightweight marker — e.g. a
`HashSet<Symbol>` populated in true source order by the **textual** (non-hoist)
`RegisterDecl` site, which is the one carrying real bytecode. A `&name`
bareword TERM reference occurring inside a `constant`/`BEGIN` evaluation
consults this set instead of the plain registry: if the name isn't in it yet,
fall through to whatever the OUTER scope (or CORE) had for that name instead
of the not-yet-reached local declaration.

- Cheapest, most targeted; smallest blast radius.
- Requires a real "what did the outer scope have" fallback to exist — for
  `postcircumfix:<[ ]>` specifically this means shipping a native core `Sub`
  value for the operator (§2's third bullet), otherwise the fallback has
  nothing to resolve to.
- Does NOT fix the redeclaration-scope-blindness in §1.2's plain-sub case
  (that's a compile-time static check, a different code path, needing its
  own scope-awareness fix) — would leave that as a separate, still-open bug.
- Risk: another special-cased side table living next to the registry it
  shadows is exactly the kind of dual-mechanism CLAUDE.md flags as technical
  debt, though narrower in scope than a full rewrite.

### Option B — a genuine two-pass compile (stub-declare pass, then source-order body-compile pass), dropping the hoist-pass's *runtime opcode emission* in favor of pure compile-time bookkeeping

Mirror real Raku's actual model: a compile-time-only symbol table (no runtime
`RegisterDecl` opcode) records every sub name in the block up front so
*call-site* resolution can always find a forward reference (this is a
compiler-side lookup, not a registry entry) — while the runtime
`RegisterDecl` opcode is emitted **once**, at the sub's own textual position,
in true source order. `&name` bareword-reference resolution then needs no
special casing at all: it just reads whatever the registry holds at that
point in execution, which is now genuinely "what's been declared so far",
because nothing was pre-registered out of order.

- Architecturally the correct fix — removes the dual
  hoisted-placeholder/textual-backfill mechanism entirely rather than adding
  a third table next to it.
- Also fixes §1.2's redeclaration-scope-blindness for free, IF the
  compile-time symbol table is itself scope-aware (nested block scopes get
  their own table, chained to the enclosing one) — folding two related bugs
  into one coherent mechanism, same spirit as ADR-0039 §4.
- Large: touches `hoist_sub_decls`, `RegisterDecl` opcode emission sites,
  `hoisted_sub_plans`/backfill bookkeeping, and the redeclaration-check call
  sites in `registration_sub.rs` — a compiler-pipeline restructuring, not a
  local patch. Also has to prove no other feature silently depends on the
  hoist-pass's *runtime* side effects (e.g. a `RegisterDecl`-driven trait or
  export mechanism firing at block entry rather than at the textual site).

### Option C — special-case `&postcircumfix:<...>`/operator-name bareword resolution only

Hardcode operator-name bareword lookups inside `constant`/`BEGIN` to always
resolve to a synthesized native dispatcher, ignoring any user override
regardless of position. Rejected outright: violates the project's "no
special-case logic / hardcoded results" rule, and does not generalize to the
equally-affected plain-sub case demonstrated in §1.2 — the bug is not about
operators.

## 4. Recommendation

Option B is the architecturally sound fix and the one this ADR recommends
once resourced — it eliminates the dual-registration mechanism instead of
growing a third table beside it, and it closes the plain-sub
redeclaration-scoping bug (§1.2) as the same mechanism rather than as a
separate follow-up. It should be scoped and planned together with, but not
merged into, ADR-0039 — that ADR owns `@`/`%` container *value* lexical
resolution; this one owns *sub-name* lexical resolution, a different runtime
structure (`registry().functions` vs `env`) with a different current
mechanism (a hoist+backfill pass vs by-name `HashMap<String, Value>`
sharing), even though the diagnosis is the same shape.

This ADR is **investigation-only** — no slice plan, no acceptance-criteria
checklist, and no implementation was attempted in the session that filed it.
The postcircumfix ticket's item 2 (todo/deep) stays open, updated to point
here instead of restating the narrower "no native Sub value" framing it
started from.

## 5. Status

Proposed. Not started. A future session picking this up should first decide
between Option A (narrow, ships a working `&postcircumfix:<[ ]>` fallback
sooner, leaves §1.2's redeclaration bug open) and Option B (the real fix,
larger), and should re-verify §1.2's redeclaration-check code path (its exact
location in `registration_sub.rs`/`compiler/mod.rs` was traced by symptom,
not read line-by-line) before committing to a design.

## 6. Re-investigation 2026-09-04: the crash is fixed; two of this ADR's premises were wrong

The motivating symptom (§1.1) — a stack overflow from the `Array::Rounded`
delegation idiom — is fixed, and **not by either option below**. Everything in
this section was measured on `main` (b0a4fdae0) against `raku` v2026.06.

### 6.1 What was actually wrong, and what fixed it

`&postcircumfix:<[ ]>` was never on the operator fast path in
`resolve_code_var` (`src/runtime/accessors_resolve.rs`), which already routed
`infix:<...>` / `prefix:<...>` / `postfix:<...>` terms to a by-name routine
reference whose call path (`vm_dispatch_helpers.rs`) deliberately gives the
CORE routine priority over a same-named user declaration. So the analogous
idiom for an infix has always worked:

```raku
class F {}
my constant &oldadd = &infix:<+>;
multi sub infix:<+>(F $a, F $b) { "F+F" }
say oldadd(1, 2);        # 3        (CORE, not the user candidate)
say F.new + F.new;       # F+F
```

`postcircumfix` fell off that path and dropped through to the generic
`has_multi_keys` branch, which materialises *the user's own candidates* by
value — so `old-same` was the very candidate that called it. The hoist pass is
what made those candidates visible that early, which is why §1.2 read the
failure as a hoisting bug; but the hoist is not what made the captured term
*wrong*. Even a textually-preceding user candidate would have been captured
there, and would have recursed identically.

Two further gaps had to close for the term to be usable at all (§2's third
bullet): mutsu had no CORE subscript routine, so `postcircumfix:<[ ]>(@a, 1)`
answered "Unknown function" and `&postcircumfix:<[ ]>` answered `Nil`. Both
subscript operators are now ordinary builtins
(`src/runtime/builtins_postcircumfix.rs`), driving the same opcode the syntax
lowers to, with the user-candidate probe in `exec_index_op_with_positional`
suppressed for exactly that one dispatch — the CORE candidate performs native
indexing and must never re-enter the override delegating to it.

Pinned in `t/core-postcircumfix-routine.t` and
`t/user-postcircumfix-core-delegation.t`.

### 6.2 Premise that was wrong: "plain subs are the scope-blind case"

§1.2 says mutsu's redeclaration check "is name-based and scope-blind for plain
subs, while operator names are apparently exempted". Measured, it is the other
way round. A plain single sub shadows correctly:

```
sub foo() { "outer" }
{ sub foo() { "inner" }; say foo(); }   # raku: inner   mutsu: inner
say foo();                              # raku: outer   mutsu: outer
```

It is `proto`/`multi` that are not lexically scoped at all: an inner-block
`proto sub foo` raises a false `Redeclaration of routine 'foo'`, and an
inner-block `multi sub foo(Int)` *merges* into the outer candidate set
(`Ambiguous call to foo(Int); these signatures all match: (Int $x), (Int $x)`)
where raku shadows. That is a distinct defect with its own repros; it is
tracked separately and is not a prerequisite for anything here.

### 6.3 Premise that was wrong: Option B is not safe as specified

§4 recommends Option B — emit each `RegisterDecl` at its own textual position
so the registry is genuinely "what has been declared so far". Measured against
raku, that model is incorrect, because the discriminator is **compile time vs
run time**, not textual position at run time. raku installs a sub's pad entry
at compile time, so an ordinary *runtime* reference sees the whole scope
regardless of order, while only a `BEGIN`-time evaluation sees a partial one:

| source | raku | mutsu today |
|---|---|---|
| `sub foo(){"outer"}; { my constant &old = &foo; say old(); sub foo(){"inner"} }` | `outer` | `inner` |
| `sub foo(){"outer"}; { my $old = &foo; say $old(); sub foo(){"inner"} }` | `inner` | `inner` |
| `say f(); constant X = 1; sub f(){42}` | works | works |
| `constant X = f(); sub f(){42}; say X` | compile error | `42` |

Row 2 and row 3 are exactly what Option B would break: emitting `f`'s
registration in source order makes a runtime reference *before* that point stop
resolving, which raku does not do. Option B is therefore rejected as written.
Any future fix must key off "is this reference being evaluated at BEGIN time",
information the compiler has at the reference site, not off the position of the
registration.

### 6.4 What is still open

Rows 1 and 4 of the table above are unfixed: a `&name` (or `name(...)`)
reference inside a `constant` initializer or a `BEGIN` block still sees
declarations that only the hoist pass has made visible. That is the residue of
this ADR, and it no longer blocks the postcircumfix idiom that motivated it.
It is worth noting why it is not merely "Option A": with the registry keyed by
fully-qualified `Package::name`, an inner-block declaration *overwrites* the
outer entry for the duration of the block, so suppressing the not-yet-reached
declaration does not by itself reveal what raku would have found (row 1 wants
the outer `foo`). Making that work needs the sub registry to carry lexical
scope, which is the same missing mechanism §6.2's proto/multi shadowing bug
needs, and the same shape ADR-0039 describes for `@`/`%` containers. Those
three should be resourced as one campaign, not three patches.

## 7. Campaign status 2026-09-04: the lexical-scoping half is done; the BEGIN-time half is not

§6.4 asked for the proto/multi shadowing bug, the container-side analogue and
the BEGIN-time residue to be resourced as **one campaign**. The first of those
is now closed, in four slices, and the finding is that it did **not** require
giving the registry lexical scope. Three separate mechanisms were each blind to
one key shape or one boundary:

1. **`our` is not lexically shadowable.** `register_proto_decl`'s lexical-shadow
   exemption was keyed on lexical nesting alone, so a nested `our proto`
   redeclaring a package-scoped one was accepted where raku refuses it. Gated on
   `!is_our` (`news/2026-09/our-proto-is-not-lexically-shadowable.md`).
2. **`do { ... }` did not scope its routine declarations.** Every other block
   form takes the `snapshot_routine_registry`/`restore_routine_registry` pair;
   `OpCode::DoBlockExpr` took none of it, so a `do`-block `sub` permanently
   replaced an outer one. The opcode grew a compile-time `scope_routines` flag
   (`news/2026-09/do-block-scopes-its-routine-declarations.md`).
3. **A package body's family was invisible, then merged.** `run_class_body`'s
   `class_subs` tail probe tested the exact `Pkg::name` key and so never saw a
   `multi`'s `Pkg::name/arity:types` candidates, leaving method dispatch at
   `current_package = GLOBAL`; and the bare-name candidate gathers pooled every
   package in `bare_name_packages()` into one ranking, merging two independent
   families across a package boundary
   (`news/2026-09/package-body-multi-is-lexical-to-the-package.md`).

The lesson for §6.4's remaining work: "the registry needs lexical scope" was too
big a diagnosis for this half. The registry's *key shape*
(`Pkg::name` vs `Pkg::name/arity:types`) and its *snapshot boundary* were the
real gaps, and both were fixable at the mechanism that already owned them.

**Still open**, unchanged: rows 1 and 4 of §6.3's table — a `&name` reference
inside a `constant` initializer or a `BEGIN` block still sees declarations that
only the hoist pass has made visible. §6.3's rejection of Option B stands: the
discriminator is compile time vs run time, not textual position. Also newly
filed from this campaign:
`todo/tickets/our-multi-in-a-package-body-cannot-see-its-own-our-proto.md`,
a *registration-order* defect in the CHECK-time inline-package prepass.
