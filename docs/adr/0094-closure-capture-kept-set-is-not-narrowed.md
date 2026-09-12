# ADR-0094: The closure capture's kept set is not narrowed — its cost is the call-time merge

- **Status**: Accepted (2026-09-12)
- **Related**: [ADR-0086](0086-builtin-dynamics-are-not-closure-capture-material.md)
  (the one family that *was* removed from the kept set, and the structural
  argument that let it go); [ADR-0092](0092-closure-capture-is-a-chained-tier-not-a-merged-copy.md)
  (the call-time merge's shape — this ADR supplies the measurement its §5 asks
  for); [ADR-0084](0084-the-frame-env-is-not-the-programs-symbol-table.md)
  (the frame `Env` is not the program's symbol table — where the remaining
  type-name half belongs); [ADR-0018](0018-slot-addressed-lexical-capture-and-env-sync.md)
  (why the *lexical* half of a capture is already O(free variables))
- **Addresses**: [#7557](https://github.com/tokuhirom/mutsu/issues/7557), the
  item left after #8075 — "the kept set is still over-broad for the other
  families it keeps by construction (uppercase-initial user lexicals,
  `?`-twigil pseudo-lexicals, `self`/`?CLASS`)"

## 1. Context

`Interpreter::capture_closure_env` keeps, besides the closure's free variables,
**every visible env key that is not a plain user lexical**
(`env::is_plain_user_lexical` — anything whose first character after the sigil is
not a lowercase ASCII letter). mutsu stores scalars sigil-less, so a user's
`my $Foo` and a bare type name `Foo` are the same env key *shape*; the rule is
deliberately conservative and keeps, as collateral, every uppercase-initial user
lexical, constant and type name in scope.

#7557 has been worked in five rounds before this one. Its fixed per-creation
costs are gone (Part A, #7624, #7682, #7855), the body and signature are shared
rather than cloned (Part C, #7682), and the family that dominated the kept set — the ~20 built-in
dynamics — left the env entirely for a per-interpreter base tier (ADR-0086,
#8075). What was left was the narrowing above, held back each time because it
needs an argument that ADR-0086's does not supply: there is no call-side proof
that a type name is re-supplied.

This ADR measures it instead of arguing about it. A capture today
(`MUTSU_DUMP_CAPTURE` instrumentation over `capture_closure_env`):

| creating scope | entries | keys |
| --- | --- | --- |
| mainline of an empty program | 3 | `=pod ?FILE Any` |
| inside a one-parameter sub | 7 | `=pod ?FILE @_ Any _ __mutsu_callable_id n` |
| `bench-ctor`'s `*.flat`, inside `TWEAK` | 11 | + `! %_ ?CLASS Dist Spec __ANON_STATE__ self` |
| `bench-yaml-parse`, inside a grammar action | 42–63 | 33 of them `YAMLish::*` / `Node` / `Tag` / … type names |

So in real code the bulk of what remains is **type and package names**, one per
declared class/role/grammar plus its short alias, and they scale with the
program's own declarations.

## 2. Measurement 1 — narrowing is worth nothing on the creation side

The experiment is the *maximal* narrowing, correctness deliberately ignored:
drop every uppercase-initial identifier-shaped key (`[A-Z][A-Za-z0-9:-]*` — type
names, constants, enum members and uppercase user lexicals alike) from every
capture unless it is a free variable. Nothing sound can beat it, so it bounds
the whole family. Callgrind, one binary toggled by an env var, the predicate a
bit on the flags word the filter already loads (`MUTSU_JIT=off MUTSU_GC=off`,
release, precompilation cache warmed):

| | kept set intact | maximal drop | |
| --- | --- | --- | --- |
| `bench-ctor` | 1,311,811,895 | 1,305,686,250 | −0.47% |
| `bench-yaml-parse` | 583,828,370 | 583,604,651 | −0.04% |
| `bench-class` | 1,140,246,498 | 1,140,187,085 | −0.01% |
| `bench-grammar-parse` | 47,560,611 | 47,526,333 | −0.07% |
| `word-count` | 1,041,914,583 | 1,042,046,305 | +0.01% |
| `my $c = * + 1;` × 200000 | 1,896,327,806 | 1,883,735,825 | −0.66% |
| the same + 30 enclosing uppercase lexicals | 3,703,643,403 | 2,849,318,533 | **−23.1%** |
| 30 classes, `sub make($n) { my $c = { $n + 1 } }` × 20000 | 1,102,183,918 | 1,046,694,481 | **−5.0%** |

`bench-yaml-parse` captures 33 type names per closure and does not care. The
reason is #7624's memo: the filter's result is a pure function of the visible env
and the chunk, so an unchanged scope hands back the same map. Function-level
shares make it explicit — the filter closure plus `filtered_flat_capture` cost:

| | filter + walk | what it means |
| --- | --- | --- |
| `bench-yaml-parse` | 114,022 Ir (0.02%) | the memo hits; the 42–63 entries are built once |
| `my $c = * + 1;` × 200000 | the filter does not appear at all | memo hits; whole capture is 117 Ir/creation |
| `bench-ctor` | 8,890,025 Ir (0.68%) | memo misses, but only 3 of 11 keys are in this family |
| 30 classes in a sub frame | 75,181,072 Ir (6.83%) | memo misses (a fresh overlay per call) **and** the scope is wide |

**The kept set costs O(kept env) per creation only where the memo cannot hit**,
which is the sub/method-frame shape #7855 documented — and then ~142
instructions per kept key per creation.

## 3. Measurement 2 — the kept set is not free on the *call* side

`call_compiled_closure_in_unit` merges the captured env into the callee frame's
overlay, one key at a time, on **every call**, with no memo. So the kept set
sizes that loop too. Two programs that differ only by 30 empty classes declared
in the mainline, calling one stored closure 200000 times:

| | with 30 classes | without | difference |
| --- | --- | --- | --- |
| kept set intact | 5,650,202,206 | 4,735,585,897 | +914,616,309 |
| maximal drop | 4,820,504,765 | 4,710,394,002 | +110,110,763 |

The 110M that survives the drop is the one-time cost of declaring 30 classes.
The rest is the merge: **4,022 instructions per closure call, 134 per captured
name**, for names the closure never mentions. Where it goes, per call, for those
30 keys:

```
1788  call_compiled_closure_in_unit   (the merge loop itself)
1643  Env::contains_key_sym           (the don't-overwrite probe, whole chain)
 403  entry_or_insert_sym_with
 275  Value::view_kind
 128  Env::insert_sym                 + 128 set_shared_var_sym, 70 set_env_plain_lexical
```

That is ADR-0092 §1.1's cost, and this measurement adds the part that ADR was
explicit about not having: it is **not only a floor**. ADR-0092 measured 4,831
instructions per call of which 954 scaled with a `use Test` import list; 30
ordinary class declarations add 4,022 more on top of the floor. A closure call
costs O(enclosing scope), and the enclosing scope here is just "how many classes
the file declares".

The inline `.map`/`.grep` consumer path does not pay it (ADR-0018's per-consumer
slot sets): the same 30 classes cost 11.96M of 1,118M (+1.1%) on
`@a.map({ $_ + 1 })` and the drop recovers 1.8M of that (−0.16%). The shape that
pays is an explicitly-called closure — a callback in a variable or attribute,
`my &f = …; f()`, a dispatch table.

## 4. Decision

**Do not narrow the kept set.** Specifically:

1. **The sound narrowing is worth ~2 keys.** The only discriminator between a
   type name and an uppercase user lexical that does not depend on completing a
   static analysis is the *value*: a type/package name, enum member or routine
   binding is name-ish, `my $Foo = 42` is not. That keeps every type name — the
   entire bulk of §1's table — and drops uppercase user lexicals and unreferenced
   constants, one or two keys in a realistic program. It also costs the filter
   its **key purity**, which is the soundness premise of both memos that make
   §2's numbers what they are (`vm_capture_cache`'s address comparison and
   `env_tier`'s key-set index). A ≤2-key win is not worth spending that premise.
2. **The unsound narrowing is not ours to spend.** Dropping a type name not
   named in the body means deciding free-variable analysis is authoritative for
   bare-word reads, which it is not: `op_name_const_idx` does not include
   `GetBareWord`, and completing it means enumerating every op and signature
   position that resolves a type name at runtime. Its failure mode is not loud —
   `exec_get_bare_word_op` falls back to the global class registry, which answers
   *correctly for a package-scope class*, so a missed route silently degrades a
   lexical `my class` alias or an import alias into a registry lookup. That is
   the "incomplete static analysis" shape CLAUDE.md warns about, spent on a
   ~5% win in one synthetic shape.
3. **The cost it was proxying belongs to the merge.** ADR-0092 §2 makes the
   capture a tier the callee's env consults instead of merging it key by key.
   That removes §3's 134 Ir/key/call for *every* family at once — dynamics that
   ADR-0086 has not moved, `?CLASS`, `self`, `__mutsu_*` metadata, type names —
   with no argument about what a body can name. §3 is the measurement ADR-0092
   §5 asked for before committing to it.
4. **The type-name half has a structural home, not a filter home.** A
   package-scope `class Foo` is program-global (`our`-scoped in Raku) and its env
   entry is an alias to a globally-registered name; it is in a frame's `Env`
   because ADR-0084 has not finished moving the symbol table out. Moving it is
   what makes the capture stop carrying it — the ADR-0086 argument, applied to
   the family ADR-0086 could not reach, and the reason it was not done here is
   that `dyn_base` is an immutable `Arc<SymMap>` installed once, while class
   declarations run during execution.

#7557 is closed on this. Its title claim — creation costs O(enclosing env) — is
now false for creation (§2) and true for calls (§3), and the call side is
#7565/ADR-0092's.

## 5. Also measured, and useful to whoever comes next

**The headline metric.** `my $c = * + 1;` × 200000 against the same loop with
the creation removed, wall clock, release, JIT on, best of 5:
mutsu **0.63 µs** per creation against rakudo's **0.096 µs** — 6.6x, from 15.4x
when #7557 opened (2.47 vs 0.16). The control loop itself is 5.7x *faster* than
rakudo's, so the whole program wins; the per-creation gap is real but it is no
longer the capture. Per creation, 6,024 instructions, diffed against the control
loop:

```
1110  exec_set_local_op_inner   (storing the closure into `my $c`; an ordinary
                                 store of an Int in the control loop is 746)
 353  exec_make_lambda_op
 594  malloc + _int_free
 246  exec_one_dispatch
 212  RawTable::clone
 206  Value::view_kind
 204  Env::get_sym
 200  exec_set_var_dynamic_op   (the by-name env write a `my` declaration makes,
                                 which is also what keeps churning the tier the
                                 capture memo pins)
 117  capture_closure_env       (the whole capture, memo hitting)
```

The single largest item is the store, not the capture: 746 of those 1,110
instructions are what *any* local store costs, self cost, in a ~2,000-line
store-flavour cascade that a plain scalar assignment falls through in full. Filed
as [#8094](https://github.com/tokuhirom/mutsu/issues/8094) rather than folded in
here.

**Two methodology traps, both of which cost a round.** (1) An expensive
experimental predicate hides the effect it is meant to measure: the first
version of §2's filter resolved the symbol and scanned its bytes per key, which
made dropping 31 of 38 captured entries come out 1.1% *slower*. Put the
experiment on a bit of the flags word the filter already loads. (2)
`bench-yaml-parse` and `word-count` load modules and mutsu caches precompilation
under `~/.cache/mutsu`; a cold first run attributed +191M Ir to the change under
test. Warm each binary once and discard it.

## 6. Alternatives rejected

- **Complete the free-var scan for bare-word reads** (add `GetBareWord`, typed
  declarations, signature type names, trait names to a per-chunk name-reference
  set, then drop any uppercase key not in it). Sound in principle and it is what
  the maximal drop in §2 approximates; rejected on the ratio — an open-ended
  enumeration whose miss mode is quiet (§4.2) against ≤0.5% on every real
  benchmark.
- **A per-tier memo of the filter's positive verdict** (extend
  `env_tier::capture_candidates` from "keys no capture can keep" to "keys every
  capture keeps", so a memo-miss capture stops re-deciding each key). Sound, no
  semantics, and it attacks the ~60 of the ~142 Ir/key that is the *decision*
  rather than the copy. Rejected for now on the same ratio: its ceiling is the
  0.68% the filter costs on `bench-ctor`, the only real benchmark whose memo
  misses. Worth revisiting if #7856 (`Symbol::flags()` costs 33 Ir for what
  should be an array load) is fixed first and the remainder is still visible.
- **Hoist `Any`, `=pod` and `?FILE` — the three-entry floor — into the base
  tier** the way ADR-0086 hoisted the dynamics. `Any` qualifies (a `Value::NIL`
  sentinel seeded once at startup); `=pod` is written during execution and
  `?FILE` is per-compunit and mirrored on the `Env` itself
  (`Env::source_file_sym`), so the floor cannot go below 2. One entry is ~134 Ir
  per closure call and nothing per creation — real, but not worth a base-tier
  change on its own.
