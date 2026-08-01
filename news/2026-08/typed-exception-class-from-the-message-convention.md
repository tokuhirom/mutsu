# A compile-time error keeps its exception class instead of collapsing to `X::AdHoc`

`throws-like` and a typed `CATCH { when X::Foo {…} }` both dispatch on the
exception's *class*. mutsu raised most compile-time errors as an untyped
`RuntimeError` whose message merely *spelled* the class — `"X::Obsolete:
Unsupported use of . to concatenate strings; in Raku please use ~"` — and the
untyped-error path wrapped that whole string in an `X::AdHoc`. The type was
right there in the text and nothing read it:

```
$ mutsu -e 'try { EVAL q{"a" . "b"} }; say $!.^name; say $!.message'
X::AdHoc
X::Obsolete: Unsupported use of . to concatenate strings; in Raku please use ~

$ raku -e 'try { EVAL q{"a" . "b"} }; say $!.^name; say $!.message'
X::Obsolete
Unsupported use of . to concatenate strings. In Raku please use: ~.
```

`RuntimeError::exception_value()` now makes the `"X::Type: text"` convention
real: it prefers a structured exception the error already carries, then splits
the convention out of the message, and only then falls back to `X::AdHoc`. A
name is accepted only when every `::`-separated segment looks like a type name,
so `die 'X:: is the exception namespace'` stays an `X::AdHoc` with its sentence
intact. The class name is dropped from the resulting `.message`, matching raku.

The same function replaces four hand-rolled copies of the
`e.exception … else X::AdHoc` fallback (`vm_try_catch_ops`, two in
`vm_misc_scope`, `fail_error_to_failure_value`), so the rule now lives in one
place rather than being restated at each conversion site.

## Why it surfaced now

mutsu's *native* `Test` provider matched `throws-like`'s expected type against
the message text, so these assertions passed anyway. rakudo's real
`Test.rakumod` uses `nqp::istype`, which does not. The full Test-vendoring sweep
(`todo/tickets/vendor-real-test-module.md`) ran every `t/*.t` file twice — once
against the native provider and once against the vendored upstream module — and
this single root cause accounted for 29 of the 57 files that regressed under the
real module while passing under `raku`. **20 of those 29 are cleared by this
change** — the whole sweep goes from 86 regressions to 64, and from 2617 to 2641
files passing under both. The remaining 9 are errors whose message does not name
a class at all (a parse failure reported as `Confused. parse error at …` where
raku raises `X::Syntax::Malformed`), which is a separate piece of work.

Pinned by `t/typed-exception-class-of-compile-errors.t`, green under `raku` too;
3 of its 14 assertions fail without the change (the other 11 pass either way,
because the native provider is lenient — which is the whole point).
