# A method added with `.^add_method` keeps its own name

`$D.^add_method('m', my method dm { ... })` installs the code under `m`, but
the `Method` object is still `dm`: rakudo's `.^find_method('m').name` and
`.^methods` answer `dm`, and an anonymous method the empty string. mutsu
named the method object after the key it was added under (#18225). The
single added candidate now records the code's own name the way
`Method.set_name` does (`Registry::rename_method_candidate`), so the method
object reports it while dispatch keeps using the key.
