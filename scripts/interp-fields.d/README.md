# Allowed `Interpreter` fields added after the baseline

`make check-interp-fields` (ADR-10779 D4) allows a direct field of
`struct Interpreter` only if it is named in the frozen
`scripts/interp-fields-baseline.txt` or in a `*.txt` file here.

A PR that extracts a subsystem into its own type adds one new file,
`<subsystem>.txt`, naming the holder field it puts on `Interpreter` (for
example `async.txt` containing `async_state`). The file's stem must be a
`SUBSYSTEMS` key in `scripts/interp-field-matrix.py`, and it is also what
classifies the field, so the PR edits neither the baseline nor the script. Each PR adds its own file and edits no shared one, so
parallel PRs cannot conflict here. Lines starting with `#` are comments.
