# `"d.add("` in a regex literal is plain text

The scanner that finds the closing quote of a double-quoted regex literal treated every `.name(` as the
start of a method-call argument list (so that `"@a.join(",")"` can hold a quote inside the call). A
literal such as `"data.addColumn('number', '" $name "')"` therefore never found its closing quote:
the pattern did not match and, when it ended in the literal, the RakuAST round trip refused it.
JavaScript::Google::Charts' `t/scatter-column-names.rakutest` failed four assertions on it. A
`.name(` now opens an argument list only when an interpolated variable, or a postcircumfix chain on
one, precedes the dot.
