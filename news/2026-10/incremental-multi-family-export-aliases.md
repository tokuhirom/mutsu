# Exporting a multi family no longer re-aliases the whole family per candidate

Each `multi` candidate of an exported family used to call
`register_exported_sub`, which scanned the whole function registry for the
family's `pkg::name/…` keys and rebuilt every `EXPORT::<TAG>::name/…` alias of
every candidate found so far. A family of c candidates cost O(c²) alias inserts
and O(c·r) registry-key visits, often two or three times per candidate.

Registering a multi candidate now reports the registry keys it was installed
under. Once the family's tags are recorded, the export path aliases only those
keys; the registry scan runs only when a family's export is first recorded (or
gains a tag), so candidates declared before it are still exported. Re-recording
an already-present tag no longer copies the copy-on-write export tables, and the
remaining scan no longer allocates a `String` per registry key.

On `use Test; ok 1;` (profiling build, warm precomp cache, callgrind) the export
path visits ~4,300 registry keys instead of ~36,000, its inclusive cost falls
from ~11.5M to ~3.2M instructions, and the whole program from 114.3M to 100.6M
instructions (#11761).
