# App::RaCoCo loads: unit-role exports, imported short-name parents, `mkdir`

Three gaps kept App::RaCoCo's reporters from loading and its tests from running:

- `unit role A::B::Reporter is export;` now publishes the short name `Reporter`
  to the importer, as `unit class A::B::C is export` already did.
- A `unit class A::B::Kid is Base` / `does Reporter` whose parent is a short
  name the file itself imported (`use A::B::Base;`) now resolves it: the `use`
  files the alias under the package that was current when it ran, which the
  class header's own package chain did not reach.
- The `mkdir` sub answers the directory's `IO::Path` (or the `Failure` the
  method answers) instead of `True`, so `sub f(--> IO::Path:D) { mkdir $p }`
  type-checks.

All 26 provided modules load; 11 of the 21 test files now match rakudo and the
rest are triaged in #13126.
