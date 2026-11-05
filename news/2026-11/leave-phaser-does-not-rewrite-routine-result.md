# A LEAVE phaser no longer rewrites a routine's returned value

`sub f { LEAVE Cfg.level = "info"; Cfg.level = "trace"; Cfg.level }` returned
`info`: the tail (a call of an `is rw` method) left its container on the value
stack and the LEAVE phaser wrote through it before the caller read the result.
A routine that returns no container (no `is rw` / `is raw` / `return-rw`) now
reads its tail value before the leave queue runs; an `is rw` routine and a bare
block still hand out the container, as in rakudo.
