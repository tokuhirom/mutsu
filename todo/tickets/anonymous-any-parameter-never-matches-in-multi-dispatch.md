# An anonymous `Any` parameter never matches, so `multi f(Any)` is dead code

Found while fixing `nextsame` in a single-candidate multi
(`news/2026-08/nextsame-in-the-only-candidate.md`). It is a **separate root
cause** — it lives in argument matching, not in the dispatcher stacks — so it
was deliberately not folded into that slice.

## Symptom

A multi candidate declared with an **anonymous** (type-only, no variable name)
`Any` parameter never matches any argument at all:

```raku
multi a(Any) { "A" }
say a(42);     # raku: A          mutsu: Cannot resolve caller a(Int:D) ...
say a("s");    # raku: A          mutsu: Cannot resolve caller a(Str:D) ...
```

Every other type works — only `Any` is affected:

```raku
multi b(Cool) { "C" }; say b(42);    # both: C
multi c(Str)  { "S" }; say c("s");   # both: S
multi d(Mu)   { "M" }; say d(42);    # both: M
```

The user-visible consequence is that the idiomatic `Any` fallback candidate
silently disappears, which shows up most often as a *redispatch* that goes
nowhere:

```raku
multi w(Int) { "int:" ~ callsame() }
multi w(Any) { "any" }
say w(1);        # raku: int:any     mutsu: "Use of Nil in string context", int:
```

Naming the parameter (`multi w(Any $x)`) hides the bug entirely, which is why
this reads at first like an "anonymous parameters do not redispatch" problem.
It is not: anonymity only matters because it selects the code path below.

## Root cause

The parser gives an anonymous type-only parameter the placeholder name
`__type_only__` (`src/parser/stmt/sub_param/param_inner.rs`). In
`src/runtime/types/args_matching.rs` (`args_match_param_types`, the
`pd.name == "__type_only__"` arm, ~line 348) that name is taken to mean "a bare
identifier term — e.g. an enum value — so resolve it from the environment and
compare the argument against it":

```rust
} else if pd.name == "__type_only__" {
    // Bare identifier param (e.g., enum value) -- resolve from env and compare
    if let Some(expected_val) = self.env.get(&resolved_constraint).cloned() {
        if dispatch_arg != expected_val {
            return false;
        }
    } else if !self.type_matches_value(&resolved_constraint, &dispatch_arg) {
        return false;
    }
}
```

`Any` is the one type name that *is* present in the environment:
`src/runtime/runtime_init.rs` (~line 3117) installs a sentinel
`interpreter.env.insert("Any".to_string(), Value::NIL)`. So the env branch wins,
the argument is compared against `Nil`, and every non-`Nil` argument is
rejected. `Cool`/`Str`/`Mu` have no such env entry, fall to
`type_matches_value`, and behave correctly.

## Suggested fix

Only take the env-term comparison when the constraint is *not* a resolvable type
name. `Interpreter::is_resolvable_type` (`src/runtime/types/type_registry.rs`,
~line 838) exists for exactly this discrimination and is already used for
`__type_only__` params in `src/runtime/types/binding_signature.rs:225`:

```rust
} else if pd.name == "__type_only__" && !self.is_resolvable_type(&resolved_constraint) {
    // ... env-term comparison ...
} else if ... /* fall through to the ordinary type check */
```

The bug is one condition wide, but the blast radius is not: this arm is on the
hot multi-dispatch path and is what makes anonymous *enum-value* parameters
(`multi f(Less) {...}`) dispatch, so the change wants its own targeted roast
sweep over `S06-*`, `S12-*` and the enum tests, plus
`scripts/battery-testsuite.sh`.

## Repro

`raku` and `mutsu` disagree on all three lines:

```raku
multi a(Any) { "A" }
say (try { a(42) }) // "NOMATCH";      # raku: A     mutsu: NOMATCH
multi w(Int) { "int:" ~ (callsame() // "Nil") }
multi w(Any) { "any" }
say w(1);                               # raku: int:any   mutsu: int:Nil
```

No roast file in the current `MUTSU_REAL_TEST=1` residue gates this, which is
why it is filed rather than fixed.
