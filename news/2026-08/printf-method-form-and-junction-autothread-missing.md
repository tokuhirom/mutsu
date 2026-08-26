# `printf` gains its method form and its `Junction` candidate

Two divergences from `Type/independent-routines.rakudoc:687,692`, both in
`printf`: the documented method form `$format.printf(*@args)` died with
`No such method 'printf' for invocant of type 'Str'` (mutsu even suggested
`print`), and `printf("%.2f ", 1/3 | 1/4 | 3/4)` died with
`Directive %f not applicable for type Junction` instead of printing
`0.33 0.25 0.75`.

## Root cause 1 — the method form was never registered

`sprintf` had a `Cool`-method arm in `dispatch_method_by_name_1` that prepends
the invocant to the argument list and delegates to `builtin_sprintf`; `printf`
had only a zero-argument arm (`"str".printf`, which prints the invocant). The
missing arm is the same shape plus a write to `$*OUT`. It is guarded on the
receiver *not* being an `Instance`, so `$*OUT.printf(...)` and
`IO::CatHandle.printf(...)` keep their own handle-writing dispatch.

## Root cause 2 — the autothreading rule is narrower than "directives autothread"

The ticket's hypothesis was that the format-directive matcher needs a Junction
branch. Asking Rakudo what it actually declares says otherwise:

```
$ raku -e 'say &printf.candidates>>.signature.raku'
(:(Str(Cool) $format, Junction:D \j), :(Str(Cool) $format, |))
$ raku -e 'say &sprintf.candidates>>.signature.raku'
(:(Str(Cool) $format, *@args),)
```

So the autothreading is *dispatch*, not formatting, and it is asymmetric:

- `printf` has an explicit `Junction:D \j` candidate, so **exactly one** Junction
  argument threads (`printf("%.2f ", 1/3|1/4|3/4)` returns
  `any(True, True, True)` and prints three times). Two Junction arguments fall to
  the slurpy candidate and die — verified.
- `sprintf` has no such candidate: `sprintf("%d", 1|2)` dies. mutsu must keep
  dying there, and does.
- The *format* parameter is `Str(Cool)` in both, and a Junction autothreads
  through that coercion, so `sprintf("%s"|"[%s]", 5)` is `any("5", "[5]")` and
  `printf("%s"|"[%s]", 5)` prints `5[5]`.
- `Cool.printf` is `:(Cool $:: *@args, *%_)` — no `Junction:D` candidate — so the
  *method* form must NOT autothread an argument. The new arm therefore calls
  `builtin_sprintf` directly rather than routing through `call_function`.

mutsu had a format-Junction branch in both `sprintf` and `printf`, but it was
wrong in a way no test caught: it concatenated `.Str` of the junction's members
and discarded the remaining arguments, so `sprintf("%s"|"[%s]", 5)` returned the
literal `"%s[%s]"`. Both now thread the format properly, re-running the whole
format per eigenstate and returning a `Junction` of the results.

## The bug the first attempt uncovered: the format was never coerced

Threading the format per eigenstate initially made `roast/S16-io/print.t`
("Str-using routines do not thread Junctions") fail, and the reason turned out
to be a separate, pre-existing gap rather than anything about Junctions.

Both sprintf entry points read the format as

```rust
let fmt = match args.first().map(Value::view) {
    Some(ValueView::Str(s)) => s.to_string(),
    _ => String::new(),          // <-- silently empty
};
```

so a format that was not *already* a `Str` produced the empty string:
`sprintf(42)` was `""` where raku gives `"42"`. That is the `Str(Cool) $format`
coercion simply not being applied. It went unnoticed because the old
Junction branch stringified the eigenstates itself before ever reaching this
code; threading exposed it, since each eigenstate of that roast test's junction
is a `Cool` subclass whose `.Str` is a user method.

`Interpreter::builtin_sprintf` now coerces a non-`Str` format through
`render_str_value` (which dispatches a user `.Str`), and the pure-native
`native_sprintf` fast path — which cannot dispatch one — returns `None` for a
non-`Str` format so the interpreter-aware path takes over. A bare type object
keeps its existing `""`, matching today's behaviour. With that in place the
roast file passes with *cleaner* output than before: the format junction's
eigenstates now render as their own `.Str` rather than leaking `all(`/`any(`
gist fragments into the printed text.

A Junction *invocant* of `.printf`/`.sprintf` is the format, so it autothreads
too; both method arms route a Junction receiver through `call_function` where
that threading lives.

## Result

`$fmt.printf(...)` works, `printf($fmt, $junction)` autothreads and returns a
`Junction` of `Bool`s, `sprintf` autothreads a Junction *format* but still
rejects a Junction *argument*. Pinned by `t/str-coercion-and-dispatch.t`, which
redirects `$*OUT` to a temp file so it can assert on what was written.
