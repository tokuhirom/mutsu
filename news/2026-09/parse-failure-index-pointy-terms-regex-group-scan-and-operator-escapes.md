# Parse-failure index: pointy terms, the regex group scanner, and operator-name escapes

Three more constructs off the `blocked_load` parse-failure index (#7954), reached
the way the earlier batches were: fetch the tarball named by the `ecosystem/dists/`
record, `--dump-ast` every module the META6 `provides` names, bisect the failing
file down to the smallest still-failing range, and reduce that to a one-liner
checked against rakudo. Seven distributions stop failing to parse.

## A sigilless pointy parameter is a term, in every control statement

`Hash::Ordered` reported `expected ')'` at a `method DELETE-KEY(::?ROLE:D: \key)`
header, and the pseudo-type invocant it names parses fine on its own. The cause
was seven lines further down, inside that method:

```raku
with %!indices.DELETE-KEY(key) -> \index {
    %!indices.AT-KEY(@!keys.AT-POS($_))-- for index .. @!keys.end;
}
```

`-> \index` declares a **term**: inside the block a bare `index` IS that binding.
Only `for` and `while` carried that into the body's parse scope, so in a `with`
the body was parsed against what `index` means *outside* — the `index` builtin —
and the listop swallowed the rest of the statement. The block then failed at its
closing brace, and the reported location was the enclosing `method`.

The registration is one shared helper now (`block_with_pointy_params`), used by
`if` / `elsif` / `unless` / `else` / `given` / `with` / `without`, and it also
registers a `&name` parameter as a routine for the same reason `for` already did
(`-> &m { m() }` must read `m()` as a call, not an `m//` match).

Registering the name was not enough on its own. The parse memo is keyed by the
input slice alone, and `if`'s header runs the full parameter-list parser, which
speculates over the `{ ... }` that follows it — so the body's failure had already
been memoized *before* the name existed, and the correctly-scoped parse was
handed that stale entry straight back. The scoped body parse therefore runs in
its own parse generation, which is precisely the scope's effect on the grammar.

Two silently-wrong answers fell out of the same investigation. `with 1 -> \v { v }`
declared an ordinary lexical, so the body's bare word was a package lookup and
the block read `(Any)` where rakudo reads `1`; the binding now carries the
sigilless marker that makes it a term (and read-only, as rakudo has it). And
`if 0 -> \a { } else -> $x { }` handed the `else` clause `Nil`: a clause's
`binding_var` keeps the declaration's own spelling, `\a` included, and the else
lowering stripped only a `$`, so it read a name nothing declares.

Unblocks `Hash::Ordered`, and `Archive::Ar` / `BSON::Simple` / `Terminal::Tests`,
which all failed on the same line of it.

## The regex group scanner reads `<...>` by one rule where there are two

The scanner that finds a capture group's closing `)` tracked a single `<...>`
nesting depth and keyed every other decision off it. But a character **class**
and an **assertion** read their contents by opposite rules, and this scanner owes
both:

- `<[.)]>` is a class, so its `)` is a literal member. Counting it ended the
  group early and `( \d+ <[.)]> )` died on the leftover `]` — the ordered-list
  marker in `Markdown::Lex` and `Blogin`.
- `<!before '>}}'>` is an assertion, so the quote really does open a string and
  the `>` inside it does *not* close the assertion. Treating the quote as a
  literal member (correct for `<-['"]>`) ended it at that `>` — `Blogin`'s
  shortcode regex.

The scanner now keeps one entry per open `<`, recording whether it is a class
(`<[`, `<-`, `<+`, `<:`) or an assertion, and applies the paren and quote rules
accordingly. Balanced parens in a `<{ ... }>` code assertion are unaffected: they
cancelled out either way.

Unblocks `Markdown::Lex` and `Blogin` (all 29 of its modules parse).

## `infix:<\\>` names a one-character operator

`<...>` is a Q-style quote whose only escapes are the backslash and the
delimiters, so `sub infix:<\\>` declares the operator `\` — `infix:<\n>`, by
contrast, keeps both characters. mutsu kept the escape in the registered name, so
`MIDI::Make`'s time-signature operator went by a name nothing spells, and its own
`method time-signature (TimeSignature $ts = 4\4, ...)` default never matched it.
The angle-quoted symbol is unescaped when the name is built.

Unblocks `MIDI::Make`.

## Pins

`t/routines/signature/pointy-sigilless-param-is-a-term.t`,
`t/regex/syntax/regex-group-scan-charclass-vs-assertion.t`,
`t/lang/operators/user-infix-escaped-symbol-name.t` — all three green under
rakudo itself, so they pin rakudo's behaviour rather than mutsu's.
