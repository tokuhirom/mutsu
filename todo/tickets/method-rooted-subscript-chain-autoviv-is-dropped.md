# An autovivifying subscript chain rooted at a method call is dropped

When the root of an lvalue subscript chain is a *method call* rather than a
variable, and the chain has to autovivify the element it descends into, the
write is silently lost. Both the single-subscript and the multi-dim spelling
lose it, so this is not a multi-dim gap — it is the method-call-rooted lvalue
chain itself.

## Repro (measured 2026-09-04, debug build)

```raku
class A { has @.a }

my $o = A.new;
$o.a[0]<x> = 5;      # raku: [{:x(5)},]                 mutsu: []
$o.a[0]{1;2} = 5;    # raku: [{"1" => ${"2" => 5}},]    mutsu: []

# It only survives when the element already exists, because the popped value
# then shares its backing store with the attribute's array:
my $p = A.new;
$p.a[0] = {};
$p.a[0]{1;2} = 5;    # raku and mutsu both: [{"1" => ${"2" => 5}},]
```

`$o.h{1;2} = 5` (no chain, `has %.h`) works for the same aliasing reason: the
attribute hash is already defined.

## Why it is not a one-liner

The single-subscript compiler rewrites a method-call-rooted chain into a call to
the `__mutsu_index_assign_method_lvalue_nested` runtime builtin
(`src/compiler/expr_closure.rs`, the `Expr::Index { target: MethodCall }` arm),
which resolves the accessor and writes back by variable name. That builtin does
not install a freshly autovivified container back into the attribute, so the new
level evaporates. The multi-dim spelling has no such arm at all and falls all
the way through to `OpCode::MultiDimIndexAssignGeneric`.

The correct fix is not to grow the builtin: it is a `runtime/methods.rs`-era
slow path, and CLAUDE.md forbids routing new work through it. What is needed is
for an attribute accessor to yield a real container reference in lvalue context,
so the chain walk that
`OpCode::MultiDimIndexAssignNested` / `IndexAssignDeepNested` already perform for
a variable-rooted chain (`news/2026-09/multidim-assign-through-a-subscript-chain.md`)
can run against it unchanged. That is the "lvalue an arbitrary subscript chain"
machinery several other tickets want, and it needs a design pass rather than a
patch.

## Affected files

- `src/compiler/expr_closure.rs` — `compile_expr_index_assign` (the
  `__mutsu_index_assign_method_lvalue_nested` arm) and
  `compile_expr_multidim_index_assign` (the remaining generic fallback).
- `src/compiler/helpers_ast_utils.rs` — `index_chain_target` /
  `index_assign_target_name` stop at a `MethodCall` root.
- `src/runtime/builtins.rs` (dispatch) and
  `src/runtime/builtins_multidim_assign.rs` --
  `__mutsu_index_assign_method_lvalue_nested`'s implementation.

## Re-investigated 2026-09-04 (second pass): what is actually measured

Re-run on `main` (`cc5a39584`) against `raku` v2026.06. The repro stands, and
the surface is **wider** than this file said. Four findings change the plan.

### 1. The loss is not multi-dim-specific and not two-level-specific

```raku
class A { has @.a; }
my $o = A.new;
$o.a[0]<x>   = 5;   # raku [{x => 5}]           mutsu []
$o.a[0][1]   = 5;   # raku [[(Any) 5]]          mutsu []
$o.a[0]<x><y>= 5;   # raku [{x => {y => 5}}]    mutsu []
class B { has %.h; }
B.new.h<a><b> = 5;  # raku {a => {b => 5}}      mutsu {}
```

A single-level `$o.a[0] = 5` works. So the rule is: **a method-rooted chain of
depth >= 2 that must autovivify loses the whole write.**

### 2. Deleting the compiler arm does not help

Disabling the `Expr::Index { target: MethodCall }` arm (so the chain falls
through to the generic path) leaves all four shapes still answering `[]`/`{}`.
The generic fallback is not container-aware for a method root either, so this is
not a matter of removing a wrong special case.

### 3. The mechanism that *does* work is already there — via a name

```raku
class A { has @.a; }
my $o = A.new;
my $t := $o.a;
$t[0]<x> = 5;   say $o.a;   # [{x => 5}]  -- correct, and it reaches the attribute
$t[0][1] = 5;               # correct
```

The accessor already hands back the attribute's **shared** container (that is
why `$o.a.push(1)` works), and the variable-rooted chain walk autovivifies into
it in place and the mutation reaches the attribute. So the fix is a *routing*
problem: evaluate the accessor once, give the result a name (a synthetic
lexical), and run the ordinary variable-rooted chain against it. No new chain
walker is needed.

### 4. The blocker for that routing is the typed check — and the builtin's own
### typed check is what the variable-rooted path is missing

The builtin exists to reject `class A { has Int @.a }; $o.a[0]<x> = 5`. The
variable-rooted path did not perform that check at all (`my Int @a; @a[0][1] = 5`
silently produced `[[(Int) 5]]`), so routing the method root through it would
have traded a Tier S data loss for a permissive divergence.

**That half is now fixed** and is the prerequisite slice:
`news/2026-09/typed-array-nested-autoviv-type-check.md` — the check is read off
the container's own `ArrayData::value_type`, not off the variable's declaration,
so it is root-agnostic and fires for a `my` array, a `:=`-bound alias, or an
accessor's shared array alike. (The hash-rooted twin, `my Int %h; %h<a><b> = 5`,
already threw.)

### 5. The builtin's `Instance` branch does not work either

The `AT-POS`/`AT-KEY` branch this file's "why it is not a one-liner" section
treats as load-bearing is broken in both directions today:

```raku
class Q { has %.d is rw; method AT-KEY($k) is rw { %!d{$k} } }
class U { has Q $.query = Q.new(d => {foo => [1,2]}) }
my $u = U.new;
$u.query<foo>[0] = 99;              # raku {foo => [99 2]}
                                    # mutsu: bogus "Type check failed for an element
                                    #        of @query (no autovivification in typed
                                    #        container); expected Q but got Hash"
my $t := $u.query; $t<foo>[0] = 99; # raku {foo => [99 2]}   mutsu: silently dropped
```

The branch runs, finds no `Proxy` element, falls through, and the typed check
below it then fires on `target` (the `U` instance) whose `.query` attribute is
typed `Q` — producing a type error for a shape that has no type problem. So
preserving that branch is not a constraint on the redesign; it is a third bug.

### Remaining plan

- **Slice A (done)** — container-based typed-element autoviv check on the
  variable-rooted chain.
- **Slice B** — desugar a method-call-rooted lvalue subscript chain to
  "bind the accessor result to a synthetic lexical, then run the variable-rooted
  chain", and delete the plain-container half of
  `__mutsu_index_assign_method_lvalue_nested`. This is what closes the Tier S
  data loss.
- **Slice C** — an `Instance` root with `AT-POS`/`AT-KEY` in an lvalue chain
  (finding 5). Broken both ways today, independent of B.
