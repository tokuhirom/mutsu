# A `.map`/`.grep` block's loop phasers are compiled once, not per element

A `.map` or `.grep` block with a `NEXT` phaser compiled that phaser's body from the AST on every
element: `run_block_loop_phasers` called `Compiler::new().compile(body)` for each run. That was
~40k instructions per element before the phaser did anything. `(^200000).map({ NEXT ...; $_ * 2 })`
took 1.9 s; rakudo takes 0.28 s.

The compiled phaser is now cached per thread. The key is the block body's allocation and the
statement's index, guarded by a `Weak` that proves the entry still names that body. The
compilation reads nothing but the phaser body, so every run can use the same compiled code. The
`NEXT` map now takes 0.13 s, and a block carrying `FIRST`/`ENTER`/`LEAVE`/`NEXT`/`LAST` maps
200,000 elements in 0.44 s (rakudo 0.41 s; before this, 2.2 s).

Found while profiling ParaSeq's `t/09-map` (#17337). Filed on the way: #17673 (a `.map` block's
`FIRST` runs once per map rather than once per block).
