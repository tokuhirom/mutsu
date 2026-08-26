# `X::Cannot::Empty.new(:action, :what).message` formats its message again

Found by the doc-diff harness batch-4 re-run (`Type/X/Cannot/Empty.rakudoc:15`).

## What was wrong

rakudo's `X::Cannot::Empty` has `method message { "Cannot $.action from an
empty $.what" }` — it formats its text from `:action`/`:what` at read time.
mutsu registered the class as a plain `Exception` subclass with no such
formatter. The internal sites that throw it (`runtime/sequence.rs`,
`runtime/methods_mut_substr_buf.rs`, `runtime/methods_call_dispatch.rs`) each
pass a pre-built literal message, so they worked by coincidence — but the
documented, supported way to raise it from a custom class,

```raku
fail X::Cannot::Empty.new(:action<pop>, :what(self.^name)) unless $!next;
```

had nothing to fall back on, and `.message` answered the empty string. No
crash, no warning: a silently blank error.

## The fix

`X::Cannot::Empty` joins `X::Cannot::Lazy` and the rest of the per-type
formatter table in `src/builtins/exception_message.rs`, which computes a
`.message` from the instance's attributes. That table is deliberately shadowed
by a stored `message` attribute, so the internal throw sites keep their
literal text and only the user-constructed form changes.

Neither attribute gets a default: rakudo has none either — an omitted `:what`
stringifies to the empty string with an uninitialized-value warning, and
matching that is more useful than inventing a placeholder.

The repro from the ticket now prints `Cannot pop from an empty Stack`, as
`raku` does.
