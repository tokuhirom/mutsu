# Exporting a routine no longer scans the function registry

The first export of a `multi` family still scanned every registered function
for the family's `Pkg::name/…` candidate keys, so the export path's cost grew
with the size of the registry. That listing now comes from a routine-family
index over interned names (`qualified_tail_index::names_in_family`): every
registry key is an interned symbol, so an index folded lazily from the
symbol table's append-only id sequence is a superset of any family's keys,
with no hook at the many sites that insert into the registry. The symbol table
keeps the ids of its qualified names holding a `/` apart, so the index catches
up without visiting any other name.

The rest of the export path got cheaper too:

- alias keys are built in one reused buffer instead of one `format!` each, and
  installed under a single registry write, only when one of them is new;
- one representative key per candidate evicts its aliases from the base-name
  index, instead of one eviction per alias;
- a candidate's own `is export` no longer runs the family refresh again when
  its tags already cover every tag the family is exported under;
- once a family's tags are recorded in all four export tables, nothing is
  re-recorded, and those tables are Fx-hashed;
- the export entry points borrow the package, name and tags instead of taking
  fresh clones from every caller.

On `use Test; ok 1;` (profiling build, warm precomp cache, callgrind) the
export path (`register_exported_sub_inner`, which every export entry point and
`refresh_exported_multi_family` reach) costs ~0.97M instructions inclusive,
down from ~3.23M on the same `main`; the whole run went from 84.4M to 82.2M
(#11761).
