# A chained map fires LAST at the end; a chained multi-parameter grep takes whole groups

A `.map` chained onto another `.map`/`.grep` now fires its callback's `LAST`
phaser once, when a read sees the end of the Seq, and not when a pull merely runs
out of the upstream elements read so far. A multi-parameter `.grep` chained onto
a map cuts its pulls at whole groups, as Rakudo does.
