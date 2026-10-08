# A block's ENTER phaser now sees an `our` variable declared later

`our @log; ENTER { @log.push("enter") } @log.push("body"); say @log` printed `[body]`
instead of `[enter body]`. The ENTER phaser ran ahead of the body, before the bare `our`
declaration, so it wrote to a container the declaration then replaced. rakudo installs an `our`
symbol at compile time, so the ENTER and the body share one container.

Bare top-level `our` declarations (no initializer) are now run ahead of the block's ENTER
phasers, both for the mainline and for a module body. They only load the package container, so
running them earlier is not observable except through the ENTER.
