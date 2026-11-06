# A callback run by `REPL.repl-eval` code sees the caller's `$*OUT` and writes the caller's lexical

`Interpreter::compiler_eval` swaps `self.env` for the unit's own environment.
That dropped the caller's dynamic scope (a `$*OUT` rebound with `but role {...}`
was never reached by the unit's `print`) and discarded the write of a role
method to a file-level lexical, which only exists in the caller's env
(Text::CodeProcessing, #16977). The unit now starts with the caller's dynamic
variables, and with the caller lexicals that are shared cells (so a callback
writes the cell it captured); those cells are removed from the unit's captured
scope afterwards, so later units still do not see the caller's lexicals.
