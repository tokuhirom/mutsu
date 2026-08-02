# A resumable warning is settled where it is raised

`warn` has been settled at its raise site for a while: `builtin_warn` prints and
returns inline when no `CONTROL` handler is active, and runs a resume-safe
handler inline otherwise, so the deep computation continues exactly where the
warning came from. `raise_resumable_warning` is the shared entry point for that,
and its doc comment already warned that op-level warn sites must use it rather
than returning a bare `warn_signal_with_resume` error — because the unwinding
signal carries its resume value in `return_value`, which a routine boundary
applies as an explicit `return`, silently abandoning the rest of the body.

Several sites did exactly that anyway. `Int.Numeric`, `Nil.Str`, `Nil.abs` and
friends are *pure* native methods with no `&mut Interpreter`, so returning the
error was their only option; two interpreter-side sites (a role-composed
`Numeric` type object, and the generic type-object numeric coercion) simply had
not been converted. The result:

```raku
sub g {
    my ($did, $msg) = False;
    { Int.Numeric }();
    say "after call";                      # never ran
    CONTROL { when CX::Warn { $did = True; $msg = .message; .resume } }
    say "g did=$did msg=$msg";             # never ran
}
```

The handler fired, but `.resume` had no raise site to return to, so the rest of
`g`'s body was lost. An explicit `warn "boom"` in the same position worked.

The pure natives are now settled at their single dispatch chokepoint:
`Interpreter::try_native_method` (the `&mut self` caller of
`native_method_0arg`/`_1arg`/`_2arg`) re-raises a `warn_signal_with_resume`
result through `raise_resumable_warning`. The two interpreter-side sites call it
directly. When no resume-safe handler is active the fallback is the same
unwinding signal as before, so nothing changes for `CATCH`-style handlers.

This is roast `Test::Util`'s `warns-like` shape: it calls the code and then
declares `CONTROL { when CX::Warn { … .resume } }` in the same block, so under
the real module every `warns-like { Int.Numeric }` — nine of them in
`S02-literals/allomorphic.t` alone — produced no test at all.

Pin: `t/warn-resumes-at-the-raise-site.t`, byte-compatible with `raku`.
