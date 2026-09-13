# "Two terms in a row" now tells you where

`PError::fatal_at` exists precisely so a fatal parse error carries the failure position, and
`parse_program` then reports `at <file>:<line>` with the `------>` source echo. Five of the ten
"Two terms in a row" sites still raised the position-less `PError::fatal`, so the whole diagnosis
was one line of text:

```
Parse error: Confused. Two terms in a row
```

Six distributions in the ecosystem sweep failed with exactly that and nothing else —
`Physics::Unit`, `Red`, `RedFactory`, `Shell::DSL`, `Test::Declare`, `Test::Describe` — for four
different reasons, each of which had to be prefix-bisected by hand before it could even be named.
`Test::Declare` took a bisect to land on line 6 of an 18-line file.

## What changed

Four sites moved from `PError::fatal` to `PError::fatal_at`, threading the unconsumed rest that was
already in scope at each:

- `src/parser/stmt/modifier.rs` — a bare word starting the next line (`check_two_terms_across_lines`)
- `src/parser/stmt/modifier.rs` — a term after a `for` modifier's iterable
- `src/parser/primary/ident/identifier_call.rs` — an identifier butted against a quote (`foo'bar'`)
- `src/parser/primary/misc/reduction.rs` — a listop reduction butted against its operand (`[+]@a`)

A **fifth** site needed more than a swapped constructor. `src/parser/stmt/decl/has_decl.rs` raises
through `PError::fatal_with_exception`, which had no position field at all — and that site is the
one behind the ticket's own headline example, `class C { has $.x is rw when 1; }` from
`Test::Declare`. So `fatal_with_exception_at` was added alongside it. The rest of the machinery
already handled the case: `render_parse_error` computes line/column from `remaining_len` and then
copies them onto the exception's own attributes, so `$!.line` / `$!.column` now resolve there too
rather than staying unset.

Before and after, on the ticket's example:

```
$ mutsu t.raku
Runtime error: Confused. Two terms in a row

$ mutsu t.raku
===SORRY!=== Error while compiling t.raku
Confused. Two terms in a row
at t.raku:2
------>class C { has $.x is rw when 1; }
                               ^
```

## Pin

`t/exceptions/two-terms-in-a-row-reports-position.t` — 15 tests, three per site, asserting the full
shape end to end (the diagnosis, `at <file>:<line>`, and the `------>` echo) rather than merely that
an error is raised. Each case buries the offending construct a few lines in, so a default position
of 1 would fail. All five constructs are genuine "Two terms in a row" errors in rakudo too, and the
file passes under real Rakudo as well as under mutsu.

## One divergence found and left alone

For the across-lines case both implementations report line 4, but rakudo echoes the line the
statement *began* on (`------> 42 if 23<HERE>`) while mutsu echoes the continuation line
(`------>is 50; 1`) — so for the common cause, a missing `;`, rakudo's caret is where the fix goes
and mutsu's is not. That is out of this ticket's scope and is filed as
[#8329](https://github.com/tokuhirom/mutsu/issues/8329); the pin deliberately accepts either echo
rather than freezing mutsu's choice as if it were the spec.
