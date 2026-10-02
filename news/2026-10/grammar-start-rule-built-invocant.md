# A grammar's start rule runs on a built instance

`.parse` now calls its start rule on `self.new`, as rakudo does (#10848). The
start rule's own code blocks see a built grammar instance — `has $.n = 1` reads
`1` there, BUILD and TWEAK have run, and a `has $.x is required` attribute makes
`.parse` die with `X::Attribute::Required` — while every subrule's blocks keep
running on a cursor minted without BUILD, where the same attribute reads `Any`.
Even `G.new(n => 7).parse(...)` gets a fresh `G.new`. A method-shaped start rule
(`method TOP { ... }`) gets the same built instance as its `self`.

The parse arms the invocant; the first engine run of the start rule's pattern
takes it — the walk's start-rule scope, or the compiled engine's root frame,
which publishes it for each `Code` op it runs — and hands it back when the run
ends, so a subrule, or another parse started from a code block, never sees it.
