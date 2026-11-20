# A method exit no longer reconciles attributes

Until now, every compiled method call ended in `reconcile_attrs`. That function scanned the frame's local slots and env overlay for `ContainerRef`s, matched each one against the instance's attribute keys under seven twigil spellings, and committed a rebuilt attribute map when one looked like a `:=` binding. It existed because one kind of attribute bind did not reach the instance when it ran: binding the twigil-less alias of `has $x` (`$x := $var`). That bind planted a `!x` copy in the method's env for the exit to pick up.

That bind now seats the bound container (or value) in the instance at bind time, exactly as `$!x := $var` already did. With every attribute bind reaching the instance at once, the exit has nothing left to recover. `reconcile_attrs`, its frame scan and its ownership helper are gone (ADR-18356: the instance's attribute cell is the only store of an attribute).

Two wrong answers are fixed on the way. Both are now checked against rakudo in `t/oo/attribute/attr-bind-without-exit-reconcile.t`:

- `has $x; method b { $x := 42 }` left the attribute undefined;
- a method that bound `$x := $var` and then called another method saw the old value until the binder returned.

AttrX::Mooish's t/090 probe drops from 4,951,684 to 4,806,939 instructions per round (−2.9%, callgrind slope R=120→360). MCP::Server::Tool::Web's `html-to-text` moves by −0.24%.
