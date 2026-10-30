# The "uninitialized value $x" warning names its variable past a lexical-import marker

The names of a numeric op's variable operands are recorded against the op's index. The
compiler recorded them *before* emitting the lexical-import marker that can precede the
op, so in a unit that carries one (code compiled by the compiler object's `.eval` from a
module's method) the record pointed one op too early and the warning came out as
"Use of uninitialized value of type Any in numeric context". The record is now made after
the marker. CodeUnit's `t/01-basic.rakutest` passes.
