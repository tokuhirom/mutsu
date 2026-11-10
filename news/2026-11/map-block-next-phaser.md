# A `NEXT` phaser in a `.map` block fires per iteration

`(1..5).map({ NEXT $n++; $_ })` ran the block but never the `NEXT` phaser (a
`for` loop was fine). The inline map loops now run the block's `NEXT` phasers
after each iteration, including one ended by `next`, as rakudo does. ParaSeq's
`t/09-map.rakutest` counts them.
