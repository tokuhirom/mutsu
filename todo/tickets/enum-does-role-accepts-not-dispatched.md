# `enum ... does SomeRole` — the role's overriding `ACCEPTS` is never dispatched by `~~`, plus a spurious warning

Discovered via the doc-diff harness on `raku-doc/doc/Language/typesystem.rakudoc` (around line
846).

## Repro

```
role Weird { multi method ACCEPTS(Int:D $v) { True } }
enum Flags does Weird (A => 1, B => 2);
say 5 ~~ A;
```

- raku: `True`
- mutsu: `False`, plus a spurious `Useless use of '=>' in expression...` warning that does not
  appear for a plain `enum Flags (...)` without `does`.

## Root cause guess

Two related bugs:
1. `enum ... does Role` composes the role's methods onto the enum type, but `~~` smart-match
   dispatch on an enum value apparently always uses the built-in enum `ACCEPTS` semantics
   instead of checking for (and preferring) a role-supplied `multi method ACCEPTS` override.
2. The parser misinterprets the enum pair-list `(A => 1, B => 2)` differently when a `does
   Role` clause precedes it, producing the spurious `Useless use of '=>'` warning — suggests the
   `does Role (...)` form parses the parenthesized pair-list as a plain expression/argument list
   rather than the enum's value list.

## Affected files (starting point)

- `src/parser/` — `enum ... does Role (...)` declaration parsing
- `src/vm/vm_smart_match.rs` or wherever `~~` dispatch checks for a user-defined `ACCEPTS`
  before falling back to type/enum built-in matching

## Suggested next step

First isolate the parse-warning bug alone (`enum Flags does Weird (A => 1, B => 2);` with a
`Weird` that does *not* override `ACCEPTS` — does the warning still appear?) to determine if it's
independent of the ACCEPTS-dispatch bug or caused by the same misparse.
