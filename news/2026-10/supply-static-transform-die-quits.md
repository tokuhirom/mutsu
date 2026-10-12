# A `die` in a `Supply.from-list(...).map` stage quits the supply

`.map` and `.grep` over a materialized supply such as `Supply.from-list(...)`
run their callback eagerly when the stage is built. A `die` in that callback
used to escape as an ordinary exception. Inside a `react` it bypassed the
`whenever`'s QUIT phaser (#12986), and outside one it was thrown before
anything had tapped the supply. The stage now keeps the values produced
before the failure and ends the derived supply with a quit. A `whenever`
replays those values and then runs its QUIT phaser (LAST does not run), or
the `react` dies of the exception when there is no QUIT phaser. A `.tap`
`quit` handler receives it the same way. This matches Rakudo, where the
stage runs when the supply is tapped.
