# `f(...) = v` reports success and drops the write when the `is raw` routine hands back a value

Measured 2026-09-06 against raku v2026.07 and a debug `mutsu` built from `main`
at `1e946f7c8` plus ADR-0067's returned-container-consumers slice, which does
not touch this shape.

```raku
sub f(\x) is raw { x }
f(42) = 9;              # raku: dies "Cannot modify an immutable Int (42)"
                        # mutsu: reports success, exit 0, nothing written
```

| # | Program | raku | mutsu |
|---|---|---|---|
| R3 | `sub f(\x) is raw { x }; f(42) = 9` | dies | **`OK` — silent, exit 0** |
| R4 | `class N { has $.v = 42 }; f($n.v) = 9` | dies | **`OK`, `$n.v` still `42`** |
| R5 | `class C { has $.v is rw }; f($c.v) = 9` | `9` | `9` (fixed by ADR-0067) |

R3 is the reduced form: there is no accessor anywhere, so this has nothing to do
with the argument producer. The routine *is* rw-capable, so
`routine_is_rw_capable` correctly admits the assignment; what it hands back is a
plain `Int`, and the write should then be `X::Assignment::RO` the way
`assign_through_rw_result` already words it for the sub-lvalue path
(ADR-0059: "it handed back a plain value and the assignment is
`X::Assignment::RO` with Rakudo's 'Cannot modify an immutable <Type> (<value>)'
wording").

## Why this is its own ticket

The instance twin already refuses correctly — `$c.plain = 1` for a non-rw method
dies — and so does the *method* lvalue path when an rw-capable method returns a
value (ADR-0067's E6 controls pin that). It is only the **named-sub** lvalue
entry (`__mutsu_assign_named_sub_lvalue` ->
`builtin_assign_named_sub_lvalue` / `assign_named_sub_lvalue_with_values`,
`src/runtime/builtins_lvalue.rs`) that swallows the assignment instead of
reaching `assign_through_rw_result`'s refusal. Finding out why needs a read of
that function's fall-through, and tightening it is a behaviour change to a loud
diagnostic in a path several other spellings share (`++f()`, `f() = v`,
`substr-rw`), so it wants its own targeted roast sweep rather than being folded
into an unrelated slice.

## Repro

```raku
sub f(\x) is raw { x }
say (try { f(42) = 9; "OK" } // "REFUSED");   # raku: REFUSED   mutsu: OK
```
