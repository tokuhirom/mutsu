# `x` and `xx` read the count out of an `is rw` accessor's container

`"ab" x $obj.rw-accessor` (and `xx`) treated the accessor's container as a non-number and
repeated zero times. The repeat count is now read through the container, as for any
other numeric operand. RedFactory's `(.title ~ "\n") x .PAR("x-times")` produced an
empty body because of it.
