# A sunk list of scalar variables no longer boxes them into shared cells

`my ($i, $h) = ...` ends in a sunk `($i, $h)` collector, and compiling that list wrapped each
variable in a `WrapVarRef`, which turned the slot into a shared `ContainerRef` cell for the rest
of the routine. Every later plain store to those slots then failed the scalar-store fast lane
(`is_plain_scalar_store_slot`) and took the ~3.5K-instruction full cascade. A statement-position
list made only of plain scalar variables is now compiled without the aliasing, since nothing can
observe its element containers. The inverse loop of EC's `FiniteField` (`⁻¹`) is the motivating
case (#15981).
