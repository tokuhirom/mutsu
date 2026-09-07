# Bracketed reduction operators parse in scan form, and `R` composes with them

```raku
sub f($a, $b) { $a + $b * 2 }
say ([\[&f]] 1, 2, 3);
# raku:  (1 5 11)
# mutsu: ===SORRY!=== … Confused.
```

The ticket recorded the fold spelling (`[[&f]]`) as broken too; by the time it
was worked it already answered `11`. What was left was the **scan** form — and
it was not specific to a `Callable` at all: *every* bracketed scan spelling
failed the same way.

| Program | before | rakudo |
|---|---|---|
| `[\[+]] 1, 2, 3` | parse error | `(1 3 6)` |
| `[\[max]] 3, 1, 5` | parse error | `(3 3 5)` |
| `[\R[+]] 5, 2, 1` | parse error | `(1 3 8)` |
| `[\[&f]] 1, 2, 3` | parse error | `(1 5 11)` |

## Root cause

`flatten_bracket_op` turns the nested bracket notation into a flat operator
string — `[+]` → `+`, `R[+]` → `R+` — and it had no arm for the `\` scan
marker. `\[+]` therefore came out unchanged, and `is_valid_reduction_op`
stripped the `\` only to be handed `[+]`, which is not an operator name. One
arm (`\X` → `\` + flatten(`X`)) fixes the whole family, including `\R[+]`.

## Two runtime gaps that came out with it

- **`R` over a bracketed callable.** `[R[&f]]` flattens to `R&f`, and the
  reversal loop in `exec_reduction_op` only strips `R` when the inner name is a
  *builtin* operator, so `R&f` was looked up whole as `infix:<R&f>`. It now also
  strips when the inner is an explicit `&callable` — matched separately from the
  builtin table because a bare identifier after `R` is ambiguous (a user may
  have declared both `infix:<Rfoo>` and `infix:<foo>`) while the `&` sigil
  cannot be part of an operator name.
- **A callable that IS a builtin-operator reference.** `my &op = &[+];
  [[&op]] 1, 2, 3` died with "Unknown function: infix:<+>":
  `reduction_step_with_args` took the `ValueView::Routine` fast path and called
  the name through `call_user_routine_direct`, which looks for a user
  declaration or an `&`-keyed env binding — a core operator is neither. It now
  evaluates the operator itself in that case, exactly as the no-callable path
  does. A user-declared `infix:<+>` still wins, gated on `has_function`.

## Scope

Pinned by `t/reduce-bracketed-callable-op.t` (20 assertions measured against
rakudo 2026.07; the whole file passes under `raku` unchanged): both repro forms
and their `&`-sigil-block twins, the four bracketed-scan spellings above, the
un-bracketed forms as invariants, `R` and `\R` over a bracketed callable, the
`&[+]` / `&infix:<*>` references, a 3-arity callable (which chunks by its
arity), `@`-array and itemised-list operands, and a lazy source that must stay
lazy.

Found next door and filed rather than folded in:
`todo/tickets/a-one-or-zero-arg-reduction-with-a-user-op-does-not-call-it.md` —
`[myop] 5` answers `5` where rakudo calls the operator with one argument and
dies on arity. It is a property of every non-builtin reduction operator, not of
the bracketed spelling.
