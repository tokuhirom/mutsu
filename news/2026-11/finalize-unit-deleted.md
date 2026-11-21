# `finalize_unit` is deleted: the frontend drives the unit finalizer

ADR-15760 Amendment 3 (maintainer decision 2026-11-21, option A′) completes slice 2d. The unit's own statement list
commits at its close, which is the end of the unit. `rakuast::frontend::finish_unit_in` applies the slang action
overrides, then feeds `UnitFinalizer` the unit's top-level statements in source order, then runs the frame-state
pass. The separate `finalize_unit` function is gone.

The parser does not finalize a statement while the rest of the unit is still being parsed. A statement's
finalization reads facts that only later statements supply: a later `constant X` pins an earlier `X` to the
setting's, a later `my class DateTime` pins an earlier `DateTime` to `CORE::`, and the CORE type fold of `nqp::`
operands depends on every binding in the unit. Correcting those at the end of the unit would need retained copies
of every statement, or source-order rule changes, and there is no cost gain to pay for either. No behaviour changes.
