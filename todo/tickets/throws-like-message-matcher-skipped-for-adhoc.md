# `throws-like`'s `message => /.../` matcher is silently skipped for `X::AdHoc`

Found 2026-08-26 while writing `t/nativecall-pointer-and-cglobal.t`. This is a
**false-passing test** bug, not a missing feature: an assertion that cannot fail
is worse than one that is absent, because it looks like coverage.

## Minimal repro

```raku
use Test;
plan 1;
throws-like { die "hello world" }, X::AdHoc, message => /"ZZZ"/, "should fail";
```

- `raku`: the subtest runs 3 assertions and the third (`.message matches /ZZZ/`)
  fails, so the file fails.
- `mutsu`: the subtest runs **2** assertions (`code dies`, `right exception
  type`) and passes.

The same happens with `Exception` as the expected type, and with `gist => ...`.

## Root cause

`src/runtime/test_functions/throws_like.rs` gates every named matcher on
`has_structured_exception`:

```rust
let has_structured_exception = exception_val.as_ref().is_some_and(|ex| {
    if let ValueView::Instance { class_name, .. } = ex.view() {
        let cn = class_name.resolve();
        cn.starts_with("X::") && cn != "X::AdHoc"
    } else { false }
});
let named_checks = if has_structured_exception { named_matchers } else { Vec::new() };
```

The exclusion is reasoned ("X::AdHoc wraps ad-hoc `die()` values and doesn't
carry the attributes of the expected exception type") and is right for an
arbitrary attribute matcher such as `payload =>`. But it also drops `message` and
`gist`, which the loop below it already knows how to answer without an attribute:
they fall back to `err_message`. So the narrow fix is to let `message`/`gist`
through even without a structured exception, keeping the exclusion for every
other attribute name.

## A second, independent hole in the same function

```rust
let all_ok = type_ok && result.is_err();
```

The named-matcher results are emitted as inner TAP lines but do **not** feed the
subtest's own verdict. Worth checking whether a failing `.message matches` line
inside the subtest actually fails the file, or only makes the inner plan look
odd.

## Why this is filed rather than fixed

Fixing it will surface every currently-vacuous `message =>` assertion in `t/` and
in the whitelisted roast files at once. Some of those will be genuine mutsu
message-wording divergences that then have to be fixed or the assertions
adjusted — a bounded but real amount of follow-up work that does not belong in an
unrelated PR. Whoever takes this should expect the first CI run to be red and
should triage each newly-failing assertion rather than weakening the matcher.

## Workaround in the meantime

Inspect the message directly:

```raku
try some-failing-call();
ok $!.message.contains('expected text'), '...';
```

`t/nativecall-pointer-and-cglobal.t` does exactly this, with a comment pointing
here.
