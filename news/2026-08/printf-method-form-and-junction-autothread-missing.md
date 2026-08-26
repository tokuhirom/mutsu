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

## Result

`$fmt.printf(...)` works, `printf($fmt, $junction)` autothreads and returns a
`Junction` of `Bool`s, `sprintf` autothreads a Junction *format* but still
rejects a Junction *argument*. Pinned by `t/str-coercion-and-dispatch.t`, which
redirects `$*OUT` to a temp file so it can assert on what was written.
