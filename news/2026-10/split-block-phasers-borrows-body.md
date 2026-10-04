# A block without phasers no longer copies its body before running

Before a block, a routine body or a module mainline runs, `split_block_phasers`
separates its `PRE`/`ENTER`/`POST`/`LEAVE`/`KEEP`/`UNDO` phasers from the
statements of the body. It used to clone every statement into a new body
vector even when there was no such phaser, which is almost always the case.
A module mainline is hundreds of statements, so every `use` paid for a full
copy of the module's AST and for dropping it again.

The body is now borrowed when no phaser is split off, and copied only when one
is. On `use Test; ok 1;` (profiling build, warm precompilation cache,
callgrind) the run went from 66.27M to 64.23M instructions, and the `Test`
load cost from 57.5M to 55.5M (#11756).
