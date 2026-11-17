# A `UNIT::` read in a loaded module no longer slows every variable of the program

`mutsu` keeps a routine's locals in VM slots and mirrors them into the
name-keyed env only when something can read them by name. One process-wide latch
said "some code can read *another frame's* lexical by a run-time name". Once set,
it put every routine parameter and every `my` declaration of the whole program
on the mirroring path.

Two common module idioms set it without reaching any other frame:

- `UNIT::{"&$name"}` / `UNIT::.grep(...)` in a module's `sub EXPORT`
  (String::Utils, Identity::Utils and the many distributions built on them);
- `::("VERS::$_")`, a computed name with a constant package prefix (VERS,
  pulled in by PURL).

`UNIT::`, `MY::`, `OUTER::`, `OUR::`, `CORE::` and a package's `Foo::` stash name
a scope that lexically encloses the reading code, or a package. Like `EVAL`, they
now mark only the reading chunk, and through `static_name_lookup` the code that
declares it. A computed `::(...)` whose interpolation starts with a constant,
non-pseudo `Pkg::` prefix counts as the type lookup it is.

`my $x = $v` in a loop, per iteration (callgrind, release): 2,026 instructions
with no `use`; with `use String::Utils` 5,135 → 2,163; with `use PURL` 5,288 →
2,186. Closes #17998.

`SBOM::enums` still latches the flag: its `BEGIN ::($_)` looks up a fully computed
name, which may spell `CALLER::...`.
