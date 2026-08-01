# A regex `:my` lexical reaches its `make` block — and Cro serves a request

A regex's own `:my`/`:let` lexical was invisible to any code block containing
`make`:

```raku
my $m = "a" ~~ / :my $x = 9; 'a' { make (0, $x) } /;
say $m.ast;    # Rakudo: (0, 9)    mutsu: (0, Nil)
```

An inline block (`{ say $x }`) read it fine; only the `make`-bearing one saw
`Nil`. That is Cro's route dispatcher exactly: `Cro::HTTP::Router` builds its
path matcher by `EVAL`ing a regex of the shape

```
regex { ^ :my $req = …; :my @segs = …; :my $cap; [ … { … $cap = Capture.new(…) } … { make ($index, $cap) } … ] $ }
```

so every matched route reduced with `$cap` unset, `RouteHandler.invoke` failed
its `Capture $args` type check inside a `supply`, the failure was swallowed, and
the HTTP client hung with no response.

## Root cause

Two mechanisms have to meet, and neither knew about the other.

A declarative `:my` at the front of a pattern is **hoisted out** of the pattern
before matching (`parse_regex_declarative_prefix`), evaluated into `env`, and
restored as soon as the match returns. A code block that runs *inline* during
matching therefore just reads it from `env` — which is why the inline case
worked and looked like the feature was implemented.

A block containing `make` does **not** run inline: it needs the ordering the
bottom-up reduce walk gives, so it is recorded as a `CodeBlockContext` and
replayed later. By then the hoisted lexical has been restored out of `env`, and
`CodeBlockContext` carried no lexical state of its own — so the block read an
unset variable.

## Fix

- `CodeBlockContext` gains `regex_vars`: the in-regex lexicals as they stood at
  the block's textual position, captured when the block is recorded.
- A successful match carries the hoisted declarative lexicals on its captures, so
  the reduce walk can reinstall them.
- `reduce_run_code_blocks` (and the eager-block replay) installs those lexicals
  around each replayed block and restores the caller's own same-named bindings
  afterwards, threading a block's writes forward to the next block — the same
  ordering the inline path gets from `RegexCaptures::regex_vars`.

A `:my $*x` is deliberately excluded from all of this: it is a *dynamic*
variable owned by the per-rule dynvar machinery
(`install_fresh_rule_dynvars`), which intentionally leaves its write installed
for the action walk that follows. Snapshotting and restoring it around a block
would undo that — `t/grammar-per-match-dynvar-action.t` catches it, and
`is_dynamic_regex_var_key` now draws the line in one place.

`t/regex-my-lexical-in-make-block.t` pins the behaviour with seven assertions,
passing unmodified under Rakudo.

## Cro

With this and the owner-scoped nested-type fix
(`news/2026-08/nested-type-short-name-owner-scope.md`), **Cro serves a real HTTP
request under mutsu**: `Cro::HTTP::Server` + `route { get -> { content … } }`
answers `curl` with a complete `200 OK` response. A route with path segment
parameters (`get -> 'greet', $name { … }`) still hangs; that is tracked in
`todo/deep/cro-http-request-hang-short-name-env-pollution.md`.
