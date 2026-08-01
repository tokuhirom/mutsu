# Pod::To::Text now runs rakudo's real module, not a native reimplementation

`use Pod::To::Text` used to be recognized as a built-in no-op module, with
`pod2text` implemented as a Rust builtin walking the Pod object tree
(`Interpreter::pod_to_text`, ~100 lines in `src/runtime/io_pod.rs`). That was
BATTERIES.md rung 3 — "self-implement" — applied to a module that never called
for it. It arrived in #4541 explicitly "the same pattern as JSON::Fast", but the
pattern does not transfer: `JSON::Fast` genuinely cannot run on mutsu (the real
distribution needs ~50 nqp ops we lack), whereas `Pod::To::Text` is **168 lines
of plain Raku with no `use` statements and no nqp dependency whatsoever**. It is
not even an ecosystem distribution — rakudo ships it inside its own core
library.

The real file is now vendored verbatim at
`modules/Rakudo-Core/lib/Pod/To/Text.rakumod` (rakudo 2026.06, Artistic-2.0,
with the upstream `LICENSE` and provenance recorded alongside it in that
directory's `README.md`), and the native renderer is deleted. Output matches
`raku` character for character, including the trailing-newline behaviour the
native renderer got wrong, and `Pod::To::Text.render` works because the class is
now a real class.

## The gap was three general interpreter bugs

Running the genuine module surfaced three defects that had nothing to do with
Pod, which is exactly the compatibility signal rung 2 is supposed to buy:

**A statement ending in a closure literal did not terminate.** Upstream opens
with

```raku
my &colored = sub ($text, $) {$text }
if %*ENV<POD_TO_TEXT_ANSI> {
    ...
};
```

In Raku a statement whose text ends with a block's `}` at end of line is
self-terminating, so that `if` is a fresh control statement. mutsu absorbed it
as a *postfix modifier* on the assignment and demoted its `{ ... }` to a bare
block — which then ran unconditionally, regardless of the environment variable.
`expr_ends_with_block` (`src/parser/stmt/modifier.rs`) already knew about
`gather`/`do`/`try` and about a call's trailing block argument, but not about a
bare `sub {...}` / `-> {...}` initializer. Pinned by
`t/closure-literal-ends-statement.t`.

**`$=pod` was collected out of heredoc bodies.** Pod collection scans the source
line by line and has no notion of quoting context, so a `=begin pod` — or a `#|`
declarator comment — written inside a `q:to/END/` string became a genuine
`$=pod` entry. `t/doc-mode-pod-render.t` embeds a whole document in exactly such
a heredoc to feed `is_run`, and the real `Pod::To::Text` promptly died walking
Pod nodes the program never declared. Heredoc bodies are now masked before both
scanners (`src/runtime/io_pod_heredoc.rs`, masked rather than dropped so line
numbers stay intact). This also replaced a weaker duplicate of the same idea in
`io_doc.rs`, which only understood bracket and slash delimiters and so missed
the `q:to"END"` form the failing test actually used. Pinned by
`t/pod-not-collected-from-heredoc.t`.

**`DOC INIT` blocks never saw `$=pod`** — `--doc` mode built a fresh
`Interpreter` and ran them without collecting any Pod at all, so the one
construct that exists to render the document got `Nil`. The collection step is
now a named `Interpreter::establish_pod_variables()`, but `--doc` still does not
call it: doing so exposes that a declarator's `WHEREFORE` is a bare type-name
placeholder (`Value::package("Method")`) rather than the routine object, which
makes upstream's `next unless $pod.WHEREFORE.WHY` fire outside any loop and
throw an uncatchable `X::ControlFlow`. Recorded with its measurements in
`todo/tickets/doc-init-pod-variable.md`.

## Follow-on

`Test`, `NativeCall`, `experimental` and `newline` are ordinary Raku files in
`rakudo/lib/` too, and `modules/Rakudo-Core/` now exists to hold them. Each is a
candidate to retire from native provision the same way.
