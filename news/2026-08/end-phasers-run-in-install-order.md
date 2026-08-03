# A module's END phaser runs *after* the script's, not before

```raku
# EndM.rakumod:  unit module EndM;  END { say "module END" }
use EndM;
END { say "script END" }
```

```
raku : script END, module END
mutsu: module END, script END
```

END phasers run in reverse *install* order, and rakudo installs one when the
compunit that declares it is compiled — so a `use` on line 1 installs the
module's ENDs before any of the script's own, and the LIFO order then puts the
script's first.

mutsu registers the main compunit's top-level ENDs **eagerly**, before the body
runs, so that they still run when the body dies. That hoist is correct on its
own (rakudo's install is compile-time too), but it put every main-compunit END
*ahead* of every module's in the registration list, and the exit-time
`.iter().rev()` then reversed the pair.

Under the real `Test.rakumod` this is not cosmetic: the module's END *is* the
plan check. Four `roast/S04-phasers` files assert from inside their own `END`,
so the plan check ran first and reported `# You planned 2 tests, but ran 1` on a
file that went on to emit both — with the missing `ok` printed immediately
after the complaint.

An `EndPhaser` now carries an `order` field recording where rakudo would have
installed it, and the exit-time loop sorts by it (stably, so registration order
still breaks ties within a class):

| class | when |
| --- | --- |
| a module's ENDs, in load order | earliest — a nested `use` installs the inner module's first |
| the main compunit's top-level ENDs, in source order | after every module |
| ENDs the main compunit registers while running (a block, a sub, an `EVAL`) | last, i.e. run first |

Which class a registration belongs to is decided by a new `module_load_depth`,
bumped around `load_module` — a depth rather than a flag because a module may
`use` another one.

`roast/S04-phasers/multiple.t`, `ascending-order.t`, `descending-order.t` and
`interpolate.t` all pass under `MUTSU_REAL_TEST=1`. Pin:
`t/end-phaser-module-order.t`, which also pins the eager registration this had
to preserve (the order still holds when the mainline dies).

One case stays approximate: a main-compunit top-level `END` written *textually
before* a `use`. rakudo installs those in source order, interleaved with the
`use`; mutsu's hoist has already registered all of them by the time any module
loads, so they sort as one block. `use` at the top of the file — every case in
`t/` and roast — is exact.
