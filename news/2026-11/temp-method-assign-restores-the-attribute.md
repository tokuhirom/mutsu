# `temp $obj.attr = v` restores the attribute, not the variable

`{ temp $r.auto-deref = False; ... }` saved the variable `$r` and put a detached copy back at the end
of the scope, so any other holder of the same object (`$holder.reader`, an array element, a hash
value, a plain alias) kept the temporized value afterwards: `True False` where `raku` prints
`True True`. The statement now saves what `$r.auto-deref` holds and assigns it back through the same
rw accessor when the scope exits, so `$r` stays the one shared object, a change made inside the scope
to another attribute survives, and nested `temp`s restore in reverse order. `temp self.x = v`,
`temp $.x = v` and a type-object invocant keep their variable/write-back restore.

Found with the PDF distribution (`t/cos-deref.t`: `temp $reader.auto-deref = False`).
