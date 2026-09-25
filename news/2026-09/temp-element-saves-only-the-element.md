# An element `temp` saves and restores just that element

`temp $t[1]<key>[1] = 23` used to save the whole base variable `$t`, copying
every level, and write that copy back over `$t` at scope exit (#9434). That cost
O(total nodes) per `temp`, and it broke identity: a name bound to the inner
array (`my $alias := $t[1]<key>`) still read 23 afterwards, because `$t` now
held a copy while `$alias` held the original. It also dropped any other write
made inside the scope (`{ temp @c[1][0] = 9; @c[0] = 5 }` lost the `5`).

The single-level forms (`temp @a[i] = v`, `temp %h<k> = v`, and `let`) had the
same shape one level down. They copied the whole container and restored it, so
a write to another element was lost, a key the `temp` created was deleted
rather than restored to `Any`, and a name bound to the element never saw the
restore.

Rakudo temporizes only the element container. mutsu now does the same: the
`let_saves` entries (`LetSaveEntry`) can record a container and a key, and the
new `LetSaveElem` opcode saves `container[key]`. The restore writes the old
value back into that element in place, through the element's own container.
When the path does not exist yet (`temp $t[1]<k>[1] = 3` on an empty `$t`), the
assignment vivifies it, and `LetSaveElemVivified` saves the new element as
having held `Any`. Only in that case are the container and key expressions
evaluated a second time. This matches rakudo, which keeps the vivified path and
restores the element to `Any`.

The whole-base `deep` save (`deep_copy_value`) and `LetSave`'s `index_mode` are
gone. `scripts/vm-complexity-check.sh` has two new cases that show both forms
flat in the size of the container.

Pin: `t/collections/temp-element-saves-only-the-element.t`.
