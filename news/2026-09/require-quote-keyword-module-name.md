# `require Q;` loads a module named after a quote keyword

`require Q;` (and `q`, `qq`, `qw`, `m`, `s`, `rx`, `tr`, `y`, ...) was parsed as a quote
construct using `;` as its delimiter, swallowing the next statement into the module name. The
`require` module-name parser now treats these identifiers as plain module names.
