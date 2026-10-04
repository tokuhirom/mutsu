# Lookarounds run on the compiled regex engine

`<?before …>`, `<!before …>`, `<?after …>` and `<!after …>` used to reach the old tree walk at
every use. The compiled engine handed each lookaround to the walk's matcher, which then ran the
body. A new `Look` op now runs the body directly, as a nested run of its own compiled program.
The body shares the enclosing regex's `:my` lexicals and none of its captures, as a rakudo
lookaround cursor does.

Over `t/grammar`, `t/regex` and `t/modules`, the walk's `lookaround` leaf goes from 779 uses to
0. The `context:inline-regex-vars` declines go from 34 to 0. Every one of them was a lookaround
body in a regex that declares `:my` lexicals. This is ADR-0135 §8, Slice E, part nineteen.
