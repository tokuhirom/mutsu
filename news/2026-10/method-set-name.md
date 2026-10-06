# `Method.set_name` renames a method table entry

`$m.set_name("mm")` on a `Method` object (from `.^find_method` / `.^lookup`)
used to die with "No such method 'set_name'". It now renames the candidate: the
object itself and every later read of the class's method table report the new
`.name`, while dispatch still goes by the declared name, as in rakudo. The rename
is kept per `(class, method, candidate)` in `Registry::method_renames` (#12021).
