# A grammar `rule` with multiple embedded code blocks and subrule calls executes them out of the declared order

Discovered via the doc-diff harness on `raku-doc/doc/Language/grammar_tutorial.rakudoc` (around
line 679).

## Minimal repro

```raku
grammar G {
  rule TOP { <function-define> }
  rule function-define {
    'sub' <identifier>
    {
      say "func " ~ $<identifier>.made;
      make $<identifier>.made;
    }
    '(' <parameter> ')' '{' '}'
    { say "end " ~ $/.made; }
  }
  token identifier { \w+ { make ~$/; } }
  token parameter { \w+ { say "param " ~ $/; } }
}

G.parse('sub f ( a ) { }');
```

- `raku` (matches the doc's stated `# OUTPUT`):
  ```
  func f
  param a
  end f
  ```
- `mutsu` (`target/debug/mutsu`):
  ```
  param a
  Use of Nil in string context
  end 
  func f
  ```

Every piece of output is present but in the **wrong order**, and the first embedded block's side
effect (`make $<identifier>.made` before the second block runs) hasn't happened yet when the
second block (`{ say "end " ~ $/.made; }`) runs — `$/.made` reads `Nil` there (triggering the
"Use of Nil in string context" warning) instead of the value the first block should have already
`make`'d onto `$/`. The `<parameter>` subrule's own embedded action (`say "param " ~ $/;`) runs
*before* the first `function-define` embedded block at all, even though it's declared later in
the pattern (`'sub' <identifier> {block1} '(' <parameter> ')' ... {block2}`).

## Root cause hypothesis (unconfirmed — needs investigation)

The regex/grammar engine appears to not execute a `rule`'s sequence of subrule-calls and
embedded `{...}` action blocks strictly in left-to-right declared order during matching. This
looks different from the already-tracked "embedded code block inside a *quantified* group
doesn't persist its side effect" ticket
(`regex-embedded-code-block-quantifier-scope.md`) — this repro has no quantifiers at all, just a
plain linear sequence of literals/subrules/blocks, and the bug is about *execution order across
the whole rule*, not about a `:my`-declared variable losing writes across repetitions.

## Affected files (starting point)

- `src/runtime/regex.rs` / `src/runtime/regex_parse.rs` — the concatenation-sequence matcher for
  a `rule`/`token`, to check whether it evaluates embedded code blocks / subrule calls eagerly
  out of sequence (e.g. some kind of pre-pass or deferred-block-execution queue) rather than
  inline as each sequence element matches.
