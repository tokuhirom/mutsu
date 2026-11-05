# A regex call passes a variable to an `is rw` rule parameter

`'abc456' ~~ / <prefix_optional: $seen> /` with `my regex prefix_optional ($has-prefix is rw) { ... }`
used to die with "expects a writable container". An `is rw` plain scalar parameter now takes the
runtime parameter window (ADR-12917); an argument written as a bare `$variable` is passed with the
variable's name, and the window's end writes the parameter's final value back to it. A non-variable
argument is still `X::Parameter::RW`. Closes #16516.
