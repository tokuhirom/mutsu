# A statement modifier after a block-form phaser

`BEGIN { say "once" } for ^3;` failed with "Missing block": the phaser
statement ended at its closing `}` and the `for` was read as the start of a
new loop statement. Every block-form phaser had the same problem (`END { }
if 1`, `FIRST { } if 0`, `LEAVE { } unless $x`).

On rakudo the modifier governs only the statement's value -- its topic or
condition still runs -- while the phaser is installed unconditionally and
runs as it always does: a `BEGIN` once at compile time, a `FIRST` once per
loop, a `LEAVE` on every exit. mutsu now parses a same-line modifier after a
block-form phaser and keeps the two side by side, the phaser first, so the
phaser's own semantics are untouched (#18148). The bare form (`BEGIN say "x"
for ^2`) already applied the modifier to the phaser's statement and is
unchanged.
