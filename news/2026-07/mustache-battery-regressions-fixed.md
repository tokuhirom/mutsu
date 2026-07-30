# Template::Mustache was whitelisted but only 6 of its 13 files still passed

`batteries-whitelist.txt` records all 13 upstream `Template::Mustache` test files
as passing, but on `main` at v0.19.0 only six of them did. The release gate
(`scripts/battery-testsuite.sh`) is the only thing that runs those suites and it
only runs at release time, so the drift accumulated unnoticed:

| file | before | after |
| --- | --- | --- |
| `01-basic` | 9/10 | 10/10 |
| `06-logging` | 1/3 | 3/3 |
| `11-iterable` | 0/6 (died) | 6/6 |
| `12-inheritence` | 0/1 (died) | 1/1 |
| `50-readme` | 2/4 | 4/4 |
| `91-specs` | 3/10 | 10/10 |
| `92-specs-file` | 3/10 | 10/10 |

Three unrelated interpreter bugs were behind it. None is Mustache-specific.

## 1. `$/.from` in a pair value turned a hash composer into a block

`{ … }` is a hash composer unless its body references the topic, and mutsu
decides that with a lexical scan. Part of that scan asks whether a `.method` has
an invocant in front of it by looking at the single byte before the dot: a
letter, digit, `)`, `]`, `}` or quote means "a term ends here", anything else
means "no invocant, so this is `$_.method`".

`$/` and `$!` end in punctuation, so `{ :pos($/.from) }` was read as a topic
reference and became a `Block`. Template::Mustache's grammar action
`method tag:sym<section>($/) { make { :type<section>, …, :pos($/.from) } }`
therefore `make`s a Callable instead of a Hash, every `{{#section}}` hunk lost
its `type` key, and section nesting silently disappeared from every parsed
template.

The same byte test misreads the `/` that *closes* a term. `q:to/EOF/.trim`,
`q/x/.uc` and `/rx/.gist` all end in `/`, so a heredoc used as a hash value
(`b => q:to/EOF/.trim,` — exactly how `12-inheritence` builds its fixture) made
the surrounding literal a block. `/` before a dot now counts as a term end
unless it has whitespace on its left, which is how infix division is actually
spelled (`{ a => 1 / .elems }` stays a block).

Pinned by unit tests in `src/parser/primary/misc/lambda_tests.rs` (the decision
is a pure parser predicate, so it is asserted on the AST) plus
`t/hash-composer-term-end.t` for the heredoc spelling, which needs its body on
the following lines.

## 2. A `my` in a `when` body swallowed the matched value

A body that declares a block-local `my` runs under the `BlockLocalScope` opcode.
That opcode absorbs a `when`/`default` succeed signal, which is right for a bare
block or an `if` branch — `given 5 { if c { when Int {…} }; say "after" }` still
runs the `say`. But the same opcode wraps a `when`/`default`/`given` *body* as
soon as that body declares a `my`, and there the signal carries the value the
construct evaluates to. Absorbing it turned

```raku
when 'section' {
    my ($datum, $lambda) = get(@context, %val, :section);
    …
    elsif $datum -> $_ { when Associative { section_format $_ } }
}
```

into `Nil`, which is why rendering a `{{$ page_content }}` override produced an
empty string. `BlockLocalScope` now carries a `succeed_boundary` flag: true for
branch bodies (unchanged behaviour), false for `when`/`default`/`given` bodies,
which let the signal through to the op that knows what to do with it. Pinned by
`t/when-value-through-block-local.t`.

## 3. `sprintf('%s', $exception)` printed the object, not the message

`%s` dispatches `.Str` on an instance argument, but only when the class had an
*own* `Str` method. An Exception subclass gets its `Str` from `message` further
up the MRO, so `sprintf "%s: %s", $level.uc, $e` rendered
`Template::Mustache::X::FieldNotFound()` where rakudo renders
`Field not found ❮missing_field1❯` — the whole point of the `06-logging` file.
`%s` on an instance now goes through the same `stringify_value` that `~$obj`
uses. Pinned by `t/sprintf-instance-str.t`.
