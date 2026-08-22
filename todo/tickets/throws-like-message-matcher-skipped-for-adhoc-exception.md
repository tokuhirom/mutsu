# `throws-like`'s `message =>`/`gist =>` matchers are silently skipped for an `X::AdHoc` exception

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Test.rakudoc:586`).

## Root cause

`test_fn_throws_like` (`src/runtime/test_functions/throws_like.rs`) only runs named
attribute matchers (`message => /.../`, `payload => ...`, etc.) when
`has_structured_exception` is true:

```rust
let has_structured_exception = exception_val.as_ref().is_some_and(|ex| {
    if let ValueView::Instance { class_name, .. } = ex.view() {
        let cn = class_name.resolve();
        cn.starts_with("X::") && cn != "X::AdHoc"
    } else {
        false
    }
});
let named_checks: Vec<(String, Value)> = if has_structured_exception {
    named_matchers
} else {
    Vec::new()
};
```

`X::AdHoc` is excluded entirely because it "doesn't carry the attributes of the expected
exception type" — true for arbitrary named attribute matchers (e.g. `status =>`,
`payload =>`), which really do need real per-type attributes. But the `message`/`gist`
matchers are a special case: the code already has a working fallback for them (further
down, when `actual_val` is `None`) that reads `err_message` / calls a user `.message`
method rather than an instance attribute — that fallback works fine for `X::AdHoc` too,
it's just never reached because `has_structured_exception` filters the whole
`named_checks` list out before the loop runs.

`fail "some string"` (and a bare `die "some string"`) both produce an `X::AdHoc`
exception (confirmed: `$!.^name` is `X::AdHoc` after `try { fail "..." }`), so any
`throws-like { ... }, ExceptionType, message => /.../` against a `fail`/`die`-thrown
string message is silently dropped — the subtest plan undercounts (2 instead of 3) and
the message is never actually checked.

## Minimal repro

```raku
use Test;
sub frodo(Bool :$destroys-ring) {
    fail "Oops. Frodo dies" unless $destroys-ring
};
throws-like { frodo }, Exception, message => /dies/;
```

- `raku`: subtest plans `1..3` and includes `ok 3 - .message matches /dies/`.
- `mutsu` (`target/debug/mutsu`): subtest plans `1..2` — the `message` check never runs at
  all.

## Affected files (starting point)

- `src/runtime/test_functions/throws_like.rs` — the `has_structured_exception` gate should
  let `message`/`gist` matchers through regardless of exception class (they already fall
  back to `err_message` when there's no real attribute), while still excluding other
  named matchers for a non-structured (`X::AdHoc`) exception.
