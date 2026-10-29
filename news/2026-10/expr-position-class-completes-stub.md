# A class declared in term position is a compile-time declaration

`my $rs = model M is rs-class<R> { has $.a is column }.^rs;` declares `M` while compiling, like the
statement form: it completes an earlier forward declaration (`model M { ... }`) and applies its
attribute traits before the unit's run-time code runs. mutsu registered a term-position declaration
only when its expression ran, so `M.^create-table` before it saw a model with no columns
(`near ")": syntax error` in Red `t/23-metamodel-model`). The BEGIN prologue now lifts such a
declaration out of the unit's top-level statements as a statement of its own and leaves a read of
the type name in its place.
