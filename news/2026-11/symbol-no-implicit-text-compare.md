# A Symbol can no longer be compared with text by accident

`Symbol` exists so that a name compares as one integer. But `Symbol` also
implemented `PartialEq<str>` and `PartialEq<&str>`, so `class_name ==
"Failure"` compiled, and on every execution it resolved the symbol back to its
string and compared bytes. #19232 found about nine such resolutions on every
planned method call. The impl made the slow form the easiest one to write, and
it had spread to about 640 sites.

Both impls are removed, so comparing a symbol with text is now a compile error.
Two macros cover the cases that remain:

- `sym!("Failure")` (or `sym!(SOME_CONST)`) interns a constant name once per
  call site. Afterwards it costs one `OnceLock` load, so `class_name ==
  sym!("Failure")` is an integer compare. The argument initializes a `const`,
  so a run-time string cannot be cached by mistake.
- `sym_in!(method, "AT-KEY" | "EXISTS-KEY")` tests against several names with
  one `sym!` compare each, replacing `matches!(method.as_str(), "AT-KEY" |
  "EXISTS-KEY")`.

The ~640 implicit compares now go through `sym!`, and so do about 180 explicit
`sym.as_str() == "…"` / `matches!(sym.as_str(), …)` compares, which a
deprecation audit of `Symbol::as_str` found. `.as_str()` is left where the text
itself is the point: prefix and substring tests, splits, messages, `match`
dispatch over many names, and compares against text that exists only at run
time.
