# A trailing `;` in a call argument list now contributes its empty argument

`say('foo';)` printed `(foo)` instead of rakudo's `(foo)()`. This was reported by
the doc-diff harness against `raku-doc/doc/Language/list.rakudoc` (around line 54),
filed as "a parenthesized `;`-separated statement list drops its trailing
empty-statement element".

## The ticket's diagnosis was wrong — there are two different constructs

The ticket assumed `say('foo';)` was a *parenthesized statement list* (`(a; b; c)`)
whose trailing empty statement was being dropped. It is not, and the two
constructs behave differently on purpose. `list.rakudoc` spells this out:

> Unlike a comma, a hanging semicolon does not create a multidimensional list in
> a literal. However, be aware that this behavior changes in most argument
> lists...

- A parenthesized **term** — `('foo';)`, `(1,2; 3;)` — is a statement list. The
  final `;` is a statement *terminator*, so it adds nothing: `('foo';)` is the
  bare `Str` `"foo"`, and `(1,2; 3;)` has two elements. mutsu already implemented
  this correctly in `src/parser/primary/container/paren.rs`, which is why the
  ticket's `--dump-ast` evidence pointed at the wrong file.
- A **call argument list** — `f('foo'; 'bar')` — is rakudo's `semiarglist`: a `;`
  separates whole argument lists, each becoming one `List`-valued argument. An
  `arglist` may match nothing, so a trailing `;` genuinely opens one more, empty
  slice. `f('foo';)` passes **two** arguments, `("foo",)` and `()`; `f(;;)`
  passes three empty ones.

## Root cause

`parse_call_arg_list` in `src/parser/primary/regex/call_args.rs` already split
argument lists into `;`-separated groups, and even handled a *leading* `;`
(`f(;x)`). But its "trailing semicolon before close paren" branch returned the
groups collected so far without opening the final one, so the empty slice after
the last `;` was silently lost. One line — pushing that empty group — fixes
`f(a;)`, `f(a;b;)` and `f(;;)` alike.

Every example in the `list.rakudoc` section now matches rakudo verbatim,
including `say('foo';)` → `(foo)()` and `say(('foo';))` → `foo`, which is exactly
the pair that distinguishes the two constructs.

Pinned by `t/paren-semicolon-statement-list.t`, which also asserts that the
parenthesized-term forms and the multidimensional subscripts (`@m[1;2]`) — a
third, unrelated use of `;` — are unchanged.
