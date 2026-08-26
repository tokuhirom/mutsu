# `throws-like`'s named matchers are no longer silently skipped

`throws-like { die "x" }, X::AdHoc, message => /"ZZZ"/` passed. So did every
other `message =>` / `gist =>` / `payload =>` assertion against an exception
mutsu did not consider "structured enough" — the matcher was dropped before it
could run, and the subtest quietly planned `1..2` where rakudo plans `1..3`.
Because a bare `die "..."` and a `fail "..."` both produce an `X::AdHoc`, a
large family of assertions across the suite was passing **vacuously**: they
asserted nothing at all, and would have kept passing had the message been
anything whatsoever.

Found by the doc-diff harness (`Type/Test.rakudoc:586`) and independently
confirmed while working on NativeCall.

## Root cause

`test_fn_throws_like` (`src/runtime/test_functions/throws_like.rs`) gated the
*whole* named-matcher list behind a `has_structured_exception` flag:

```rust
let has_structured_exception = exception_val.as_ref().is_some_and(|ex| {
    if let ValueView::Instance { class_name, .. } = ex.view() {
        let cn = class_name.resolve();
        cn.starts_with("X::") && cn != "X::AdHoc"
    } else { false }
});
let named_checks = if has_structured_exception { named_matchers } else { Vec::new() };
```

The rationale — "an `X::AdHoc` does not carry the attributes of the expected
exception type" — was wrong on three counts:

1. `X::AdHoc` *does* answer `.message` and `.payload`; `die "x"` stores its
   argument in `payload`.
2. The gate also excluded every **user-defined** exception class, because
   `class MyErr is Exception` is not named `X::...`. A
   `throws-like { die MyErr.new(code => 5) }, MyErr, code => 5` never checked
   `code` at all — nor did `code => 99`.
3. Rakudo's `throws-like` calls `$x."$k"()` for every named matcher and always
   plans `2 + %matcher.elems` tests, so skipping one also makes the plan wrong.

A second, independent hole sat underneath it: the attribute lookup read
`err.exception` directly. That field is `None` for every error that carries only
mutsu's `"X::Type: text"` message convention, so even a genuinely structured
`X::` diagnosis had no attributes here — while `$!.typename` two lines outside
the `throws-like` answered correctly, because `$!` goes through
`RuntimeError::exception_value()`, which synthesizes the instance (and derives
`.what` / `.typename` / `.pre` / `.post`) from that convention.

## The fix

- Run every matcher whose value mutsu can actually produce, and count it in the
  plan.
- Resolve each matcher's value the way rakudo's `$x."$k"()` does: stored
  attribute, then a real method call on the exception, then the carrier text for
  `message` / `gist`. (`message` / `gist` invoke a method only when a *user*
  class overrides them: a built-in `X::` carrier already renders its text into
  the error message and its native `.message` can differ — `X::Phaser::PrePost`
  would regress. Every other name goes through the ordinary dispatcher, which is
  how `.payload` and `.backtrace` — native methods, not stored attributes —
  became visible here at all.)
- Read the attributes off `err.exception_value()` — the very object `$!` and
  `CATCH` see — instead of the raw `err.exception` field.
- Look through a `but role` mixin (`X::AdHoc+{X::Promise::Broken}`, what a
  broken Promise's `.result` throws) for both the attributes and the class name;
  the type-match already did this, the attribute lookup did not.
- Hand a `Callable` / `Junction` matcher the real text, so
  `message => *.contains("x")` inspects the message rather than `Nil`.

A name mutsu can produce *nothing* for is still skipped rather than failed — but
now the subtest says so out loud (`# SKIPPED matcher '.multiness': mutsu's
X::Anon::Multi carries no such attribute`), because that is a
missing-*attribute* bug rather than a bad test. The full remaining list is
tracked in `todo/tickets/exception-attributes-missing-for-throws-like.md`.

## What the un-skipped matcher immediately caught

Restoring the check turned five `t/` files red. Every one was a real mutsu bug,
and all five are fixed here — none were weakened to go green:

- **`X::Syntax::InfixInTermPosition` lost its `.infix` attribute.**
  `my @a = 1, => 2` gave `.infix` of `Nil`; rakudo gives `"=>"`. The parser does
  build the attribute, but the diagnosis is raised *softly* (so speculative
  alternatives can still back out) and only its `"X::Type: text"` message
  survives promotion. Now re-derived from the message text, the same
  derive-don't-duplicate rule already used for `X::Syntax::Missing.what` and
  `X::InvalidType.typename`. (`t/malformed-syntax-classes.t`)
- **`X::Syntax::Confused` had no `.reason` at all.** Rakudo's message *is* its
  `.reason` (default `"Confused"`); mutsu spells the same diagnosis as
  `"Confused. {reason}"`, so `.reason` is now recovered from the text —
  `"Two terms in a row"`, as rakudo reports.
  (`t/two-terms-in-a-row-initializer-listop.t`, 6 assertions)
- **`throws-like`'s string-code path reported the wrong package spelling.** Its
  nested interpreter copied the caller's classes and roles but not
  `package_kinds` / `package_stubs`, so a caller's `module M { }` was unknown
  inside it and `throws-like 'M::nope()'` produced
  `Could not find symbol '&nope' in 'GLOBAL::M'` where a plain `EVAL` of the
  same code correctly said `in 'M'`. Both registries are now copied.
  (`t/qualified-call-does-not-alias-builtin.t`)
- **`X::InvalidType.typename` and the `X::Syntax::Adverb.what` family were
  invisible inside `throws-like`** even though `$!` answered them — the
  `err.exception` vs `exception_value()` hole above.
  (`t/typed-exception-attributes.t`, `t/typed-exceptions-misc.t`)

A full local `make roast` then turned eight more whitelisted files red, and
those were real bugs too — all fixed here:

- **`:sigspace` silently dropped `<.ws>` after a `*`-quantified atom.** After
  consuming `*`, the regex parser skipped whitespace looking for a second `*`
  (the spaced `**` range form) and never put it back when there wasn't one, so
  `rx:s/col\w* 4/` compiled as `col\w*4` and stopped matching `"col 4"`. The
  whitespace skip is now committed only when the second `*` is really there.
  This is a general regex-engine bug that had nothing to do with `throws-like`;
  it merely had no un-skipped assertion pointing at it.
  (`roast/S32-str/encode.t`)
- **A broken Promise's exception is a `Mixin`**, so no attribute lookup reached
  the instance underneath. (`roast/S17-promise/basic.t`)
- **`Backtrace.is-runtime` did not exist.** Rakudo decides it by looking for a
  `SETTING::` frame; mutsu has no setting frames, so the two runtime `Backtrace`
  builders now stamp the flag and a compile-time backtrace answers falsy.
  (`roast/integration/error-reporting.t`)
- **`.return` against a pinned literal return value raised a bare "Malformed
  return value"**, naming neither the returned value nor the pinned one. Rakudo
  says `Cannot return 27 with .return when return value 42 is already specified
  in the signature`, and roast matches the exception's `payload` against `42`.
  (`roast/S32-exceptions/misc2.t`)

No assertion turned out to be merely meaningless, and no test expectation was
wrong: every expectation was already the value rakudo produces, checked against
`raku` before touching the interpreter.

## Pin

`t/throws-like-message-matcher.t` covers `message` as a `Str`, a `Regex` and a
`Callable`; `gist`; `payload`; a `fail`-produced `X::AdHoc`; a `Str` first
argument; a user-defined exception class's attribute *and* its `message`
method; and a per-type attribute on a real `X::` subclass. Crucially it also
asserts the **negative** case in a child process — that the child plans `1..3`
(not `1..2`), that a non-matching `message =>` really emits `not ok 3`, and that
the child exits non-zero. A test that only checked the passing direction would
itself have been vacuous, which is the very bug being fixed. It passes under
both `raku` and mutsu.
