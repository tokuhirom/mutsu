# The gobbled-block parse error now names the culprit and points at the line

An undeclared bareword in the reserved `X::`/`CX::` namespaces immediately
followed by a block gobbles that block, leaving the enclosing `when` without its
required one. mutsu already detected this and raised the right structured
exception (`X::Comp::Group` bundling an `X::Syntax::BlockGobbled` sorrow and an
`X::Syntax::Missing` panic, pinned by `t/comp-group-when-gobbled.t`) — but the
text a user actually saw was:

```
Runtime error: X::Comp::Group: Missing block
```

No offending name. No line. The detailed raku-style wording was built and stored
inside the exception object, then thrown away for the message, and
`PError::fatal_with_exception` left `remaining_len` at `None` so the CLI had no
position to report. Now:

```
===SORRY!=== Error while compiling tmp/gob.raku
X::Comp::Group: Function 'X::NotDeclaredAnywhere' needs parens to avoid gobbling block (or perhaps it's a class that's not declared or available in this scope?)
Missing block (apparently claimed by 'X::NotDeclaredAnywhere')
at tmp/gob.raku:6
------>        when X::NotDeclaredAnywhere { say 1 }
```

which names the same routine and the same line raku does.

## Why it came up

Found while triaging the real-dist compatibility sweep. `Raku::Pod::Render`'s
`ProcessedPod.rakumod` (1357 lines) failed with the bare
`X::Comp::Group: Missing block` and nothing else, which pointed nowhere. It turned
out **not** to be an independent mutsu parse bug: the module's
`when X::LibCurl { … }` is undeclared because its `LibCurl::Easy` dependency is
absent, and raku rejects the same file for the same reason — it just says which
name and which line. So the actionable gap was the diagnostic, not the parser.

Two method notes worth keeping, both learned the hard way here:

- **Naive prefix-truncation bisects are invalid for this file.** Braces appear
  inside regexes and interpolations (`{$entry}`, `/ '#' /`), so a brace-counting
  "closed point" heuristic cuts mid-construct; the resulting prefix fails in raku
  too. Two apparent reproductions evaporated when checked against `raku` — always
  use raku as the oracle for a candidate repro, not just mutsu's rejection.
- The same sweep triage showed 4 of its 6 "real mutsu failures" were missing
  dependencies or harness artefacts. Verify a non-`missing_dep` bucket against
  `raku -I lib` before believing it.

Pinned by `t/gobbled-block-error-names-and-locates.t` (4 `is_run` subtests on the
message text and the reported line; the exception *structure* stays pinned by
`t/comp-group-when-gobbled.t`). All 4 pass identically under raku, so it is a
parity pin rather than a mutsu-only assertion.
