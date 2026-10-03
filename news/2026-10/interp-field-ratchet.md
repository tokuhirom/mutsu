# `Interpreter` can no longer grow new fields

ADR-10779 (the `Interpreter` subsystem split, #10779) was accepted, and its
first enforcement landed: `make check-interp-fields`, part of `make checks`.
The check fails when `struct Interpreter` gains a direct field. The count
starts at 439 and is recorded in `scripts/interp-fields-baseline.txt`; it may
only go down. The check also fails when a field matches none of the subsystem
rules in `scripts/interp-field-matrix.py`.

From now on, new interpreter state goes into the subsystem type it belongs to.
A value that a caller hands to its callee is passed as a parameter, not
through a `pending_*` field. Each subsystem extraction lowers the count and
re-cuts the baseline with `scripts/interp-field-matrix.py --update`.
