# `$.attr` inside a method is an *itemized* accessor read — mutsu has both halves of the RO rule backwards

Discovered via the doc-diff harness on `raku-doc/doc/Language/traps.rakudoc`
(around line 212). The doc's own `# OUTPUT:` comment is stale (it says the trap
*should* throw "Cannot assign to an immutable value"); current `raku` does not
throw for the compound-assign form.

Re-measured against `raku` v2026.06 on 2026-08-26, and the root cause is now
**established**, not guessed. The original guess in this ticket ("raku treats a
compound-assign through a read-only accessor as a silent no-op") was the symptom,
not the mechanism.

## The mechanism

`$.x` inside a method is `self.x` **itemized** — the `$` sigil puts the accessor
result in item context, which for a bare value means *a fresh anonymous `Scalar`
container*. `self.x` is not itemized. That single difference explains every
observation:

```raku
class F { has $.x = 5; has @.a = 1,2;
  method probe { say $.x.VAR.^name; say self.x.VAR.^name } }
F.new.probe;             # Scalar   /   Int
say F.new.x.VAR.^name;   # Int      (from outside, no itemization)
```

- A **non-`rw`** accessor returns a bare value, so `$.x` itemizes it into a
  *throwaway* `Scalar`. `$.x *= 2` therefore assigns successfully — into the
  throwaway — and the attribute is unchanged. `($.x *= 2)` evaluates to `10`
  while the next `$.x` still reads `5`. No exception.
- An **`is rw`** accessor returns the attribute's real `Scalar` container, so
  itemization is the identity and `$.x *= 2` genuinely mutates it (`10`).
- A **simple** assignment `$.x = 9` is compiled as an lvalue, *without* the
  itemize wrapper (the wrapper would defeat the assignment), so it hits the raw
  accessor return: `X::Assignment::RO: Cannot modify an immutable Int (5)` for a
  non-`rw` attribute.
- `self.x *= 2` also throws, because `self.x` was never itemized.

So Rakudo's pair for a non-`rw` `has $.x`, inside a method, is:

| form | raku |
| --- | --- |
| `$.x = 9`   | throws `X::Assignment::RO: Cannot modify an immutable Int (5)` |
| `$.x *= 2`  | **silent no-op**, expression value `10` |
| `self.x *= 2` | throws |
| `$!x *= 2`  | mutates (`10`) |

## What mutsu does — both halves reversed

| form | mutsu |
| --- | --- |
| `$.x = 9`   | **succeeds** and mutates the attribute |
| `$.x *= 2`  | **throws** `X::Assignment::RO: method 'x' is not rw` |

`$.x op= v` lowers to
`__mutsu_assign_method_lvalue(self, "x", [], <computed>, "<self-var>", true)`
(`src/parser/stmt/assign/compound_expr.rs` →
`Interpreter::assign_method_lvalue_with_values`,
`src/runtime/methods_mut_method_lvalue.rs`), which rejects a public non-`rw`
accessor with "method '…' is not rw". The simple-assign form reaches the same
helper but takes the `found_public_rw` arm, which mutsu grants too generously.

## Why this was not fixed with the surrounding batch (2026-08-26)

Fixing *only* the compound-assign half — turning the throw into a silent
discard — would leave mutsu with an actively incoherent pair: plain assignment
mutates the attribute while compound assignment silently does not. That trades a
loud error for silent data loss and is worse than the current state.

Fixing *both* halves means making `$.x = v` on a non-`rw` attribute start
throwing, which is a real compatibility change for existing mutsu code and
tests. It is also the same underlying gap as the one recorded at the end of
`news/2026-08/clone-array-hash-attribute-containers-not-shared.md`: mutsu's
accessors return attribute **values** where Rakudo returns their **containers**.

## The right fix

Make the accessor return the container, then let itemization do the rest — i.e.
implement the model above literally rather than special-casing the compound
form:

1. An `is rw` public accessor returns the attribute's `Scalar` container
   (`ContainerRef` cell), not its value. This alone makes `$.x *= 2` work for
   `is rw` for the right reason, and fixes `$obj.rw-attr.VAR.^name` and the
   `=:=`-on-two-clones divergence noted in the clone news entry.
2. A non-`rw` public accessor keeps returning a bare value.
3. `$.x` as a *term* itemizes the accessor result (`Value::item()`): identity for
   (1), a fresh throwaway `Scalar` for (2). `$.x op= v` then no-ops on a
   read-only attribute with no special case anywhere.
4. `$.x` as a *simple-assign lvalue* does not itemize, so it reaches the raw
   accessor return and throws for (2).

Step 1 is the load-bearing, high-blast-radius piece; steps 3-4 are compiler
lowering. Worth an ADR before starting.

## Affected files (starting point)

- `src/runtime/methods_mut_method_lvalue.rs` — `assign_method_lvalue_with_values`
  (both the "not rw" rejection and the `found_public_rw` arm)
- `src/runtime/methods_mut_dispatch.rs` — the fast public-accessor arms
- `src/parser/stmt/assign/compound_expr.rs` — the `op=`-through-a-method lowering
- `src/compiler/expr.rs` / `src/compiler/expr_method.rs` — where a `$.`-twigil
  term would gain its itemization
