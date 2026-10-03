# A module's qualified type names and enum values no longer fill the importer's env

A module's mainline runs in the env of the frame that loads it, so every
qualified name its declarations bound stayed behind in each frame env of the
importing program. After `use Cro::HTTP2::RequestParser` that was 456 of the
~630 entries a frame env carried: the qualified names of the loaded modules'
classes, roles and subsets (`Cro::HTTP2::Frame`), and three spellings of every
enum value (`E::K`, `Pkg::E::K`, `Pkg::K`). Every copy-on-write deep copy of a
frame env copied all of them.

Declarations a module's mainline makes directly now leave the frame env alone.
A type bound under its own qualified name is no longer bound there at all,
since the type registry already answers for it; an enum value's qualified
spellings go to a per-interpreter table that bareword lookup, `::('…')`, the
package stash and the `need` scans consult after the env. A frame env is down
to ~220 entries, and a `Promise(supply { whenever … })` loop under that `use`
deep-copies 66% fewer env entries per iteration (10,057 → 3,401).

This is the second slice of ADR-0084 (#7817).
