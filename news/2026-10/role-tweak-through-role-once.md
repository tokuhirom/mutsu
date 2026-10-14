# A TWEAK from a role composed through another role runs once

`role S does R` copies `R`'s submethods into `S`'s method table, and the construction
phase walk collected the TWEAK/BUILD both under `S` and under `R`, so it ran twice per
construction. `ordered_role_submethods_for_class` now skips a copy whose origin role is
walked on its own (#12978).
