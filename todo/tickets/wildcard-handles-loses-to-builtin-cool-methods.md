# `handles *` (wildcard delegation) loses to built-in Cool/Any methods

`handles *` on a method (or attribute) is supposed to let the class intercept
*any* method call not otherwise defined and forward it to the delegate. In
`raku`, this wins even over a built-in method the object would otherwise
inherit from `Cool`/`Any` — e.g. `.uc`:

```raku
class Forward {
    method inner() handles * { 'hello' }
}
say Forward.new.uc;   # raku: HELLO (forwarded to 'hello'.uc)
```

mutsu instead resolves `.uc` through the normal built-in dispatch path before
ever reaching the wildcard-delegation fallback in
`src/runtime/methods_instance_ops.rs` ("Wildcard delegation (`handles *`) and
FALLBACK method dispatch" block, ~line 1712), so `Forward.new.uc` returns the
upper-cased default stringification of the instance (`FORWARD()`) instead of
forwarding to the delegate's `.uc`.

A method name with no built-in collision (e.g. a custom method name) is
unaffected — wildcard delegation only loses when the target method name also
happens to be a real built-in method on `Cool`/`Any`.

Reproduced identically for a plain `class`-declared wildcard handle (not just
`augment class`), so this is not walker-specific drift (ADR-0019 D3) — it is
a dispatch-ordering bug: the built-in method table is consulted before the
wildcard-delegation fallback, when `raku`'s semantics require the reverse
order for a class that declares `handles *`.

Minimal repro: `/tmp/rk9.raku` in the investigating session — recreate with:

```raku
class Forward4 {
    method inner() handles * { 'hello' }
}
say Forward4.new.uc;
```

Root cause not yet investigated in depth — needs tracing how built-in method
resolution short-circuits before the wildcard-handles fallback runs (likely
in `class_dispatch.rs`/`methods_classhow_dispatch.rs`'s owner/candidate
resolution, ahead of `methods_instance_ops.rs`'s fallback block).

## Deep-dive investigation (2026-08-10)

Root cause fully traced (gdb-confirmed), rakudo semantics established with an
oracle probe matrix, and an implementation plan follows. The original guess
(`class_dispatch.rs` / `methods_classhow_dispatch.rs` owner resolution) was
wrong — user-method resolution is fine (own/inherited/role methods already win
in mutsu). The culprit is the **native builtin fast path**
(`native_method_0arg/_1arg/_2arg`), which answers Cool-only methods for *any*
receiver by stringifying it, before dispatch ever reaches the slow-path
wildcard/FALLBACK block.

### The rakudo rule (oracle-verified table)

All probes run against system `raku` on 2026-08-10. `F` below is
`class F { method inner() handles * { 'hello' } }` (method-based) or
`has $.t handles * = 'hello'` (attribute-based) — both behave identically.

| Probe | rakudo result | Delegated? |
|---|---|---|
| `F.new.uc` | `HELLO` | **yes** |
| own `method uc` on the class | `OWN-UC` | no — own method wins |
| `uc` inherited from a user parent class | `PARENT-UC` | no — inherited wins |
| `uc` from a composed role | `ROLE-UC` | no — role method wins |
| `F.new.list` | `(F.new,)` (Any.list) | no |
| `F.new.Str` | `F<...>` (Mu.Str) | no |
| `F.new.gist` | `F.new` (Mu.gist) | no |
| `.WHAT/.isa/.defined/.Bool/.raku/.elems/.say` | all Mu/Any behavior | no |
| `F.new.split(",")` | `Cannot resolve caller split(F:D, Str:D)` | **no** — Any carries a proto, resolution *succeeds* |
| `class G {}; G.new.uc` (no handles) | `No such method 'uc' for invocant of type 'G'` | n/a — proves plain classes do not inherit Cool |
| `handles *` + `FALLBACK` on same class, `.uc` | `HELLO` (either declaration order) | wildcard **beats** FALLBACK |
| `FALLBACK` only (no handles), `.uc` | `FB:uc` | FALLBACK also intercepts Cool-only builtins |
| wildcard delegate lacks the method, class has FALLBACK | `FB:uc` | falls through to FALLBACK |
| wildcard delegate lacks the method, no FALLBACK | `No such method 'uc' for invocant of type 'M2'` — names the **delegating** class | error |
| `method x() handles <uc gist>` (explicit list) | `.uc` → `HELLO`, `.gist` → `hello` | explicit list installs a *real* method, so it beats even Any.gist |
| `class CH is PH {}` where PH has `handles *` | `HELLO` | delegation is inherited via MRO |
| `~$obj`, `$obj eq "x"`, `+$obj` | Mu.Str coercion / `Cannot resolve caller Numeric(Op:D:)` | operators never delegate |
| `F.new.can("uc")` | `()` | wildcard installs no methods |

**The ordering rule, crisply:** `handles *` (and `FALLBACK`) fire **exactly
when normal method resolution over the receiver's real MRO (class → parents →
roles → Any → Mu) throws X::Method::NotFound** — i.e. they live at the
"resolution failed" layer, wildcard before FALLBACK. A plain class derives
from Any, not Cool, so Cool-only methods (`.uc`, `.flip`, `.subst`, ...) are
*not resolvable* on it and are therefore interceptable; Any/Mu methods
(`.gist`, `.list`, `.elems`, ...) always resolve and are never intercepted.
Methods with an Any/Mu **proto** but no matching candidate (`split`, `fmt`,
`Int`, `Numeric`, `Real`) count as *resolved* — they error, they do NOT
delegate. Operators use Mu coercion, not delegation.

### Confirmed mutsu dispatch trace (gdb, debug build 2026-08-10)

`say Forward4.new.uc` — the `.uc` call takes this exact path (breakpoint on
the `"uc"` arm, `bt`):

1. `OpCode::CallMethod` → `exec_call_method_op` (`src/vm/vm_call_method_ops.rs:479`)
   → `exec_call_method_op_impl` — calls `try_native_method` at
   `src/vm/vm_call_method_ops.rs:1744` **before** any interpreter fallback.
   (The mut-op twin `exec_call_method_mut_op_impl`,
   `src/vm/vm_call_method_mut_ops.rs:2127`, does the same for variable
   receivers — both funnel into the same gate.)
2. `try_native_method` (`src/vm/vm_native_dispatch.rs:21`) →
   `try_native_method_raw` (`:38`). Its Instance-bypass arm (`:192-265`)
   checks Supply/exception/Real-Numeric/`is_native_method` — **it has no
   wildcard-handles / FALLBACK check** — so it falls through to the
   arity-keyed dispatch at `:369-377`.
3. `native_method_0arg` (`src/builtins/methods_0arg/mod.rs:304`) → the
   `try_dispatch!` chain (`mod.rs:2200-2207`) →
   `dispatch_core_numeric::dispatch`, whose arm at
   `src/builtins/methods_0arg/dispatch_core_numeric.rs:362-364` is
   `"uc" => Some(Some(Ok(Value::str(grapheme_uppercase(&target.to_string_value())))))`
   — unconditional for **any** receiver, including Instance. Result:
   `"FORWARD4()"`.uc → `FORWARD4()`. Sibling arms `lc`/`fc`/`tc` (`:365-373`)
   and the `dispatch_core_str`/`_unicode` string arms behave the same;
   n-arg builtins (`.subst`, `.contains`, ...) are answered the same way via
   `native_method_1arg`/`_2arg`.
4. The wildcard-delegation / FALLBACK block at
   `src/runtime/methods_instance_ops.rs:1743-1805` is therefore **never
   reached** for any method name the native fast path knows.

The interpreter entry (`call_method_with_values`,
`src/runtime/methods_call_dispatch.rs:51`) has the same hole: its native
fast-path call at `:2792-2801` is guarded by `should_bypass_native_fastpath`
(`src/runtime/methods_native_bypass.rs:116`), which likewise has no
wildcard/FALLBACK clause. Both gates must be fixed — if only the VM gate is
fixed, the slow path re-enters the native fast path and re-answers wrongly.

**Second bug found while tracing:** method-based wildcard delegation is
entirely broken, even for non-builtin names. `method inner() handles *`
registers the marker string `"&inner"` in `ClassDef::wildcard_handles`
(`src/runtime/registration_class_body_method.rs:333-371`), but the fallback
block at `methods_instance_ops.rs:1785` only does
`attr_var.trim_start_matches('!').trim_start_matches('.')` and then
`attributes.as_map().get(attr_key)` — `"&inner"` is not an attribute key, so
the delegate is never found and dispatch falls through to "No such method".
(Reproduced: `class FwdM2 { method inner() handles * { D.new } };
FwdM2.new.greet` → `No such method 'greet'`; raku → `hi-from-D`.
Attribute-based `has $.t handles *` works for non-builtin names.) The
explicit-list form is unaffected because it synthesizes a real forwarder
method resolved by `forward_resolved_delegation`
(`src/runtime/class_dispatch.rs:573-609`), which *does* understand the `&`
marker — mirror its delegate resolution.

Current mutsu behavior for Any/Mu methods on a wildcard instance
(`.elems`/`.list`/`.defined`/`.gist`) already matches raku (answered by
native/builtin handlers before the fallback block) — the fix must keep that.

### Fix plan

The fix = one method-name classification + one registry predicate + two
fast-path gates + repairing the `&` marker in the fallback block. No changes
to `dispatch_core_numeric.rs` arms themselves (Str/Int/etc. depend on them).

**Step 1 — add `cool_only_builtin_method(name: &str) -> bool`.**
Location: `src/runtime/methods_native_bypass.rs`, as an associated fn on
`Interpreter` (`pub(crate) fn cool_only_builtin_method(method: &str) -> bool`,
a single `matches!`). This is the set of builtin method names that rakudo
resolves **only** through Cool — i.e. the wildcard/FALLBACK-interceptable
set. Every entry below was verified against the oracle
(`raku -e "class G {}; my \$r = G.new.<M>"` → `No such method`):

```
uc lc fc tc tclc wordcase chars codes chomp chop trim trim-leading
trim-trailing flip comb words lines substr index rindex starts-with
ends-with contains subst sprintf ord chr ords Num Rat succ pred abs sqrt
sign round floor ceiling truncate base exp log log10 log2 sin cos tan
asin acos atan atan2 sinh cosh tanh asinh acosh atanh sec cosec cotan
sech cosech cotanh cis unpolar roots polymod IO lazy race hyper
samecase samemark samespace trans indent uniname uninames unival univals
uniprop uniprops uniparse parse-base parse-names NFC NFD NFKC NFKD
encode Date DateTime UInt Version
```

Do NOT include (verified to resolve on a plain Any instance, so never
intercepted): `split fmt Int Numeric Real Str Stringy gist raku perl say put
print note defined Bool so not isa does can clone new WHAT WHICH WHERE HOW
WHY VAR self item sink list List elems end flat map grep first join sort min
max minmax sum reduce unique squish repeated pick roll reverse head tail skip
batch rotor keys values kv pairs antipairs invert Array Hash hash Slip Seq
Set Bag Mix iterator eager are Capture cache classify categorize combinations
permutations pairup deepmap duckmap nodemap tree collate toggle produce
chrs`.

**Step 2 — add `class_has_wildcard_handles_or_fallback(&mut self, class_name:
&str) -> bool`.** Location: `src/runtime/class_introspection.rs`, next to
`collect_wildcard_handles` (`:468`):

```rust
pub(crate) fn class_has_wildcard_handles_or_fallback(&mut self, class_name: &str) -> bool {
    let mro = self.class_mro(class_name);
    let has_wildcard = mro.iter().any(|cn| {
        self.registry()
            .classes
            .get(cn.as_str())
            .is_some_and(|cd| !cd.wildcard_handles.is_empty())
    });
    has_wildcard || self.has_user_method(class_name, "FALLBACK")
}
```

`class_mro` is Arc-cached (`src/runtime/registry.rs:621`), so this is a short
iteration + hashmap gets. Include the FALLBACK half: the oracle shows a
FALLBACK-only class also intercepts `.uc` (`FB:uc`), and the slow-path block
already tries FALLBACK after wildcard delegation.

**Step 3 — VM gate.** In `try_native_method_raw`
(`src/vm/vm_native_dispatch.rs`), inside the Instance arm (the
`else if matches!(target.view(), ValueView::Instance { .. })` block,
`:192-265`), immediately after the `is_native_method` check (`:261-264`),
add:

```rust
// A Cool-only builtin (`.uc`, `.flip`, `.subst`, ...) is NOT resolvable
// on a plain Any-derived instance in raku, so a class that declares
// `handles *` (or a FALLBACK) must intercept it. Bail to the
// interpreter's fallback chain (wildcard -> FALLBACK -> error).
// Name gate first: the set test is a static `matches!` and free; the
// MRO walk runs only for Instance x Cool-only-name calls, which were
// previously answered by the (wrong) stringify fallback anyway.
if Self::cool_only_builtin_method(&method_name)
    && self.class_has_wildcard_handles_or_fallback(&cn)
{
    return None;
}
```

This one site covers all VM entries (plain CallMethod at
`vm_call_method_ops.rs:1744`, the mut path at
`vm_call_method_mut_ops.rs:258/563/842/2127`, autothread at
`vm_call_autothread.rs:471`, `.+`/`.*` at `vm_call_helpers.rs:318`) and all
arities (the gate sits before the arity-keyed dispatch at `:369-377`).

**Step 4 — interpreter gate.** In `should_bypass_native_fastpath`
(`src/runtime/methods_native_bypass.rs:116`), add a clause to the big `||`
chain (near the existing `has_user_method` Instance arm at `:214-215`):

```rust
|| (Self::cool_only_builtin_method(method)
    && matches!(target.view(), ValueView::Instance { class_name, .. }
        if self.class_has_wildcard_handles_or_fallback(&class_name.resolve())))
```

Required because `call_method_with_values`
(`src/runtime/methods_call_dispatch.rs:2792-2801`) re-runs the native fast
path on the slow-path route; without this the bypass from Step 3 is undone.

**Step 5 — fix the `&` (method-based) marker in the fallback block.**
Rework `src/runtime/methods_instance_ops.rs:1753-1795` so each
`wildcard_attrs` entry is parsed as `(source, optional ":regex:" pattern)`
and the delegate is resolved by marker kind, mirroring
`forward_resolved_delegation` (`src/runtime/class_dispatch.rs:590-598`):

- `source` starts with `&` → delegate =
  `self.call_method_with_values(target.clone(), &source[1..], Vec::new())`
  (on `Err`, `continue`). `inner` is a real user method, resolved before
  this block ever runs, so no recursion risk.
- otherwise → attribute read exactly as today (keep the clone-out-in-its-own-
  statement pattern; the deadlock comment at `:1762-1768` explains why).
- regex entries keep their pattern check first, then resolve the delegate by
  the same two-way rule (a method-based regex marker is
  `"&inner:regex:<pat>"` per `registration_class_body_method.rs:360-364` —
  the current code never strips the `&`).

Keep the existing order inside the block: wildcard delegation → FALLBACK →
built-in fallbacks → error (matches the oracle table, including
"delegate lacks method + FALLBACK → FALLBACK fires" via the existing
`Err(_) => continue`, and the final error naming the delegating class).

**Step 6 — hot-path guarantee (the review concern).** No new per-class cached
flag is needed. The guard is name-gated first: for every method name outside
the Cool-only set (i.e. all ordinary method calls, all Any/Mu builtins, all
user methods) the added cost is one static `matches!` that fails — effectively
free. The MRO walk only runs for the rare shape "Instance receiver × Cool-only
builtin name", which today lands in the allocating
`to_string_value()`+case-conversion stringify arm anyway. Non-Instance
receivers (Int/Str/Array hot loops) never reach either gate. Do NOT compute a
compose-time cached flag instead: `augment class` can add `handles *` after
subclasses composed, and the ticket's repro was originally found via augment —
a stale flag would be a flakiness risk (CLAUDE.md "gain vs risk").
If in doubt, compare `MUTSU_VM_STATS=1` counters on the debug build before/
after (they are optimization-level-independent); wall-clock verdicts come from
bench CI (`git show origin/bench-data:bench-history.tsv`), not local runs.

### Test file (new, `t/handles-wildcard-builtin-methods.t`)

```raku
use v6;
use Test;

plan 17;

# builtin interception (attribute-based)
class FwdAttr { has $.t handles * = 'hello'; }
is FwdAttr.new.uc, 'HELLO', 'attribute handles *: .uc delegates';
is FwdAttr.new.subst('h', 'j'), 'jello', 'attribute handles *: n-arg .subst delegates';

# builtin interception (method-based)
class FwdMeth { method inner() handles * { 'hello' } }
is FwdMeth.new.uc, 'HELLO', 'method handles *: .uc delegates';
is FwdMeth.new.flip, 'olleh', 'method handles *: .flip delegates';

# method-based wildcard with a non-builtin name (bug 2)
class Delegate { method greet() { 'hi-from-D' } }
class FwdMeth2 { method inner() handles * { Delegate.new } }
is FwdMeth2.new.greet, 'hi-from-D', 'method handles *: custom method delegates';

# variable receiver exercises the VM mut-op path
my $o = FwdAttr.new;
is $o.uc, 'HELLO', 'variable receiver delegates too';

# real methods always win
class OwnWins { method inner() handles * { 'hello' }; method uc() { 'OWN' } }
is OwnWins.new.uc, 'OWN', 'own method beats handles *';
class P { method uc() { 'PARENT' } }
class InhWins is P { method inner() handles * { 'hello' } }
is InhWins.new.uc, 'PARENT', 'inherited method beats handles *';
role R { method uc() { 'ROLE' } }
class RoleWins does R { method inner() handles * { 'hello' } }
is RoleWins.new.uc, 'ROLE', 'role method beats handles *';

# delegation is inherited
class SubFwd is FwdMeth { }
is SubFwd.new.uc, 'HELLO', 'handles * is inherited by subclasses';

# explicit handles list also intercepts a builtin
class Expl { has $.t = 'hello'; method x() handles <uc> { $!t } }
is Expl.new.uc, 'HELLO', 'explicit handles <uc> delegates';

# ordering vs FALLBACK
class Both { method inner() handles * { 'hello' }; method FALLBACK($n, |c) { "FB:$n" } }
is Both.new.uc, 'HELLO', 'handles * beats FALLBACK';
class FbOnly { method FALLBACK($name, |c) { "FB:$name" } }
is FbOnly.new.uc, 'FB:uc', 'FALLBACK alone intercepts .uc';
class Bare { }
class FwdFb { method inner() handles * { Bare.new }; method FALLBACK($n, |c) { "FB:$n" } }
is FwdFb.new.uc, 'FB:uc', 'FALLBACK fires when the delegate cannot handle';

# missing on delegate, no FALLBACK: dies naming the method
class FwdMiss { method inner() handles * { Bare.new } }
throws-like { FwdMiss.new.uc }, Exception, message => /uc/,
    'missing on delegate dies with no-such-method for uc';

# Any/Mu methods are NOT intercepted
class FwdList { has $.t handles * = (1, 2, 3); }
is FwdList.new.elems, 1, '.elems resolves on Any, not delegated';
ok FwdList.new.gist.contains('FwdList'), '.gist stays Mu.gist, not delegated';
```

Every `is` expectation above was verified against system `raku`.

### Regression hazards

- **Whitelisted roast:** `roast/S12-attributes/delegation.t` (23 `handles`
  uses; lines 190-203 pin `handles *` with own-method-wins) and
  `roast/S12-methods/delegation.t` are both in `roast-whitelist.txt` — run
  both with `MUTSU_FUDGE=1 prove -e 'target/debug/mutsu' <file>` before
  pushing.
- **Local tests touching `handles`:** `t/attr-handles-angle-word.t`,
  `t/handles-paren-strings.t`, `t/augment-method-handles-forwarder.t`,
  `t/version.t`, `t/regex-my-var-interpolation.t` (the latter two use
  `handles *`). Run them all.
- **Hot path:** the guard must stay name-gated-first (Step 6). Do not move
  the MRO walk ahead of the `cool_only_builtin_method` test, and do not add
  any check outside the Instance arms. Ordinary method calls and non-Instance
  receivers must not pay anything; bench CI (`bench-history.tsv` on
  `bench-data`) is the wall-clock authority if a perf question comes up.
- **Do not gate the fallback block itself on the Cool-only set.** Any/Mu
  methods reach it only in shapes that already behave correctly (verified:
  `.elems`/`.list`/`.gist`/`.defined` on wildcard instances match raku
  today); adding a second gate there risks breaking Package-receiver
  FALLBACK dispatch.
- **Known residues, out of scope:** (a) `Err(_) => continue` in the wildcard
  block swallows genuine exceptions thrown by a delegate method that *does*
  exist (rakudo propagates them — it checks `.can` first); (b) operators
  (`~$obj`, `+$obj`, `eq`) correctly do not delegate in either
  implementation — do not "fix" them to delegate; (c) the Cool-only set is a
  manual mirror of rakudo — when in doubt about a name, the oracle is
  `raku -e "class G {}; my \$r = G.new.<M>"` (interceptable iff it prints
  `No such method`; an arity/`Cannot resolve caller` error means it resolved
  and is NOT interceptable).
