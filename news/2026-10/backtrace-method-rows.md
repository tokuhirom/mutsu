# Backtrace and Backtrace::Frame methods are rows in the method table

`Backtrace` and `Backtrace::Frame` now have their own dispatch shapes, and
their methods (`Str`, `gist`, `list`, `flat`, `full`, `concise`, `summary`,
`nice`, `next-interesting-index`, `outer-caller-idx`, `AT-POS`, `is-runtime`,
and the frame's `subname`, `file`, `line`, `Str`, `code`, `is-routine`,
`is-hidden`, `is-setting`) are rows of the built-in method table
(ADR-11276 §9.35). The 0-, 1- and 2-argument cascades keep one call each
into the same handlers instead of their own copies of the rendering.

No behaviour change; `t/exceptions/backtrace-method-rows.t` pins the answers
as relations between them, each checked against Rakudo.
