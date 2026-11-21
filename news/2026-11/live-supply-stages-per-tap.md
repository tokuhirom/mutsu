# Live `map`/`grep`/`do`/`head` stages are instantiated per tap

In rakudo a Supply is a recipe: `$live.map(&f)` subscribes to nothing until
it is tapped, every tap runs `&f` for itself, and closing the tap (or a
`head` reaching its count) releases the source. mutsu registered the stage on
the source when `.map` was called, shared it between all taps and never
released it: an untapped `map` still ran on every emit, two taps on one `map`
ran it once, and a closed or completed tap kept the stage running for the
rest of the process (#18443).

Following ADR-18443 (accepted), a `map`, `grep`, `do` or `head` over a plain
live source is now a per-tap recipe, built on the derived on-demand Supply
machinery that already served shared on-demand sources: each tap taps the
source for itself, a dying callable quits only that tap's chain, and `Tap.close`
or a completed `head` closes the source tap. `share` remains the one eager
point. Combinators not converted yet (`batch`, `tail`, `reduce`, `zip`, ...)
and `react`'s `whenever` receive the recipe instantiated once and shared, the
eager stage they had before; later slices of the ADR convert them.

Red's live ResultSeq supply relies on this: Red 0.2.16
`t/190-resultseq-supply` tests "completed head releases its upstream tap",
"active tap materializes an insert", "closed taps do not materialize inserts"
and "simultaneous taps share materialization of one event" now pass.
