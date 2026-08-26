# `.raku` on a variable-held `Junction` renders its FIRST member with gist semantics

Found while adding `Junction` autothreading to `printf`/`sprintf`
(`news/2026-08/printf-method-form-and-junction-autothread-missing.md`) — it is
unrelated to that change and reproduces on plain `any(...)`.

## Repro

```
$ raku  -e 'my $r = any("5","6"); say $r.raku'
any("5", "6")
$ ./target/debug/mutsu -e 'my $r = any("5","6"); say $r.raku'
any(5, "6")
```

Only the **first** eigenstate loses its quoting; every later one is rendered
correctly. The bug is selective in three further ways, which is what makes it
findable:

| form | mutsu | raku |
|---|---|---|
| `my $r = any("5","6"); say $r.raku` | `any(5, "6")` | `any("5", "6")` |
| `say any("5","6").raku` (no variable) | `any("5", "6")` | `any("5", "6")` |
| `sub f() { any("5","6") }; say f().raku` | `any("5", "6")` | `any("5", "6")` |
| `my $r = any("5","6"); say $r.perl` | `any("5", "6")` | `any("5", "6")` |
| `my $r = any("5","6"); say $r.gist` | `any(5, 6)` | `any(5, 6)` |
| `my @a = "5","6"; my $r = any(@a); say $r.raku` | `any(5, "6")` | `any("5", "6")` |

So: only `.raku` (not its `.perl` alias, not `.gist`), only through a variable
receiver, only the first member. A non-string member is unaffected, and the
symptom is exactly "this one member was rendered with `.gist`/`to_string_value`
instead of `.raku`".

## Where to look

The correct rendering lives in `Interpreter::call_method_with_values`
(`src/runtime/methods_call_dispatch.rs`, the "Junction .raku/.perl/.gist/.Str
rendering" block around line 1002): it loops over `values` and calls
`.raku` on each. That path is evidently what the no-variable and `.perl` spellings
reach, and it is correct.

The variable-receiver `.raku` spelling must be taking a different, compiled
route. Candidate sites, all of which independently re-implement the
`any(...)`/`all(...)` wrapper text:

- `src/value/display.rs:957`
- `src/runtime/methods_instance_ops.rs:1927`
- `src/builtins/methods_0arg/dispatch_core_repr.rs:538` and `:683`

The "first member only" shape suggests one of these renders `values[0]` via the
junction's own stringification (or peels it off as a scalar head) before mapping
`.raku` over the rest. `rust-gdb -batch` breaking on those four sites with the
variable spelling will say which one runs in one shot — do that before editing
anything, since the same wrapper text appears in all four.

## Why it is small but not trivial

The fix itself is one render call, but there are four duplicate implementations
of Junction repr and picking the wrong one changes nothing. The real cleanup is
to make the compiled route delegate to the single `methods_call_dispatch.rs`
implementation rather than adding a fifth copy.
