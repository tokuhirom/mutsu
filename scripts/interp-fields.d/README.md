# Allowed `Interpreter` fields by subsystem

`make check-interp-fields` (ADR-10779 D4) allows a direct field of
`struct Interpreter` only if it is named in a `*.txt` file here.

Each file's stem is a `SUBSYSTEMS` key in `scripts/interp-field-matrix.py`.
When an extraction replaces existing fields with a holder field, add the
holder to the owning subsystem's file. Names of removed fields may stay so
parallel extractions do not need to remove lines. The `handoff` file records
existing side channels until ADR-10779 D3 replaces them with parameters.
Lines starting with `#` are comments.
