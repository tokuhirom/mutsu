# An EVAL'd unit with a BEGIN prologue captures its subs' variables in its own bucket

The per-run capture bucket for the named subs of an EVAL'd unit (#16371) was
switched off for a unit that has a BEGIN prologue (a `BEGIN`, a `constant`, a
`use`), because the declaring frame adopted the bucket's cells through reads that
named the mainline's key only. Those reads (`mainline_capture_at`,
`follow_mainline_redeclaration`, `unit_lexical_container_cell` and
`mainline_lexical_cell`) now ask `eval_unit_declaring_key` for the key of the
EVAL unit whose top level is running, and fall back to the mainline's. The guard
in `run_eval_unit_stmts_body` is gone, so `EVAL Q[my $x = 1; sub set-x() { EVAL
q[$x = 'str']; $x }; sub c() { my $x = 5; set-x() }; BEGIN { 1 }; c(); $x]`
answers `str` as rakudo does (it was `1` in release and a panic in debug).
