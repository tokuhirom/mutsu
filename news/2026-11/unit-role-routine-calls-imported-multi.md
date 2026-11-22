# A `unit role` file's routines reach the multis its `use` imported

A routine declared in a `unit role` file, in front of which the file `use`s a
module exporting a `multi` (with or without a `proto`), used to fail to find
it: "Routine does not have any candidates", or "Unknown function" without a
proto (#17962, found in Red's `unit role Red::Driver::Cache`). The ADR-8751
import work since then fixed it, and Red's suite is green. A regression test
now pins the three shapes the issue reported:

- a `multi` of the role file calling an imported proto family;
- a plain `sub` calling the same family;
- a `sub` calling an imported `multi` that has no `proto`.
