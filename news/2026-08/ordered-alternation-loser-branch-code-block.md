# A `||` alternation no longer runs the losing branch's code block

In `[ A || B { code } ]` the second branch's block used to execute even though
`A` matched and rakudo's cursor never reaches `B`:

```raku
my @fired;
grammar P { token TOP { 'a' [ 'b' || . { @fired.push('P') } ] } }
P.parse('ab');
say @fired;   # raku: []   mutsu (before): [P]
```

The match verdict was always right — only the side effect was spurious. But a
`{ ... }` block in a losing branch is the standard Raku idiom for "anything
else here is an error", so this turned successful parses into thrown
exceptions. `Config::TOML::Parser::Grammar` spells its escape token exactly
that way:

```raku
token string-basic-char:escape-sequence {
    \\ [ <escape> || . { die(X::Config::TOML::String::EscapeSequence.new(:esc(~$/))) } ]
}
```

so every TOML document containing a `\n`, `\"` or `\\` died with "Sorry, found
bad string escape sequence" even though `<escape>` had matched.

## Why it happened

Two mechanisms had to line up. The matcher evaluates **every** branch of a
`SequentialAlternation` eagerly — it has to, because a later branch's candidate
ends are what let an enclosing pattern backtrack (`regex r { <?> || x <r> }`
needs alt1's candidates even though alt0's zero-width match always succeeds).
And a plain `{ ... }` block that needs nothing from the reduce-time walk is a
*pure side-effect block*, which the matcher runs inline, left-to-right, as it
reaches it — so that a write to an in-regex `:my` lexical is visible to the
atoms that follow it (YAMLish's `root-block` computes its indent this way).

Eager branch evaluation plus inline block execution meant the block of a branch
raku never enters fired anyway. An existing guard covered only the degenerate
`|| { die ... }` shape (a branch that is *nothing but* a block); a branch that
merely contained one, like `|| . { die ... }`, walked straight past it.

## The fix

A new thread-local, `SPECULATIVE_ALT_BRANCH`, is set while evaluating any
branch that an earlier branch of the same ordered alternation already beat.
While it is set, a plain side-effect block becomes a zero-width no-op.

Skipping it cannot change the candidate set — such a block always succeeds — so
backtracking still sees exactly the same branch ends it did before. `<?{ ... }>`
/ `<!{ ... }>` assertions deliberately keep running: their result decides
whether the branch matches at all, so suppressing them *would* change the
candidates.

Pinned by `t/ordered-alternation-loser-code-block.t`, which checks all three
shapes (literal, subrule, nested-token first branch), that the block still runs
when its branch is the one that matches, and the `die`-in-the-losing-branch
shape from `Config::TOML`.
