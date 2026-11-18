# ParaSeq: `last` in a hyperized map stops it, and `FIRST` fires once

Three general bugs kept ParaSeq's `t/09-map`, `t/47-deepmap` and `t/49-nodemap`
from matching rakudo (#18024).

**`=:=` / `nqp::eqaddr` on bound names of other objects.** ParaSeq marks the
batch in which a `last` ran with `$!lastsema := $semaphore` and later tests each
batch with `nqp::eqaddr($next, $!lastsema)`. #18084 made a bound attribute its
value to container identity; the named-operand comparison still answered
`False` for any object other than an Array, Hash or class instance (a Promise,
a `ConcBlockingQueue`), so the test never matched and every batch after the
`last` was still delivered. Two names with no Scalar of their own now compare
the objects they hold (`my $p := Promise.new; my $q := $p; $p =:= $q` is True,
as on rakudo).

**`FIRST` in a `.map` callback with ENTER/LEAVE/LAST.** Such a callback is
compiled as a block scope so its LEAVE-type phasers fire on every exit. Its
`FIRST` went into that block too, where it runs on every entry, so
`{ FIRST ...; ENTER ...; $_ }` fired `FIRST` once per element. `FIRST` now
stays outside the block, on the callback's own once-per-clone flag.

**`Block.callable_for_phaser` shared its block's identity.** The callable it
hands out copied the block's id, which keys an anonymous block's wrap chain.
ParaSeq binds `$!do` on its mapper clone for the batch that runs `FIRST`, so
the `LAST` callable it calls at the end ran the whole block instead: one extra
call with no argument, and the `LAST` phaser itself never ran. The callable is
now a code object of its own, as in rakudo.
