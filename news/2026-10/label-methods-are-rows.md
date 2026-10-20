# `Label`'s methods are method-table rows

`Label`'s `name`, `file`, `line`, `Str`, `gist`, `raku`, `next`, `last` and `redo`
are rows of a closed `Label` dispatch shape (ADR-11276 slice 5); the loop-label
adapter at the top of the zero-argument native cascade is gone. The I/O and
concurrency owner lookups that two by-name dispatch entries spelled out as the
same six-way chain are one `Interpreter::io_concurrency_owners_of`.
