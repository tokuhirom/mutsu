# Method calls on the parameters of an imported `is rw` multi

An `is rw` multi imported from a module ran with its parameters as container
cells. `$s.chars` on such a parameter failed with `No such method 'chars' for
invocant of type 'Str'` (the mutating-call path did not read a scalar cell
through), and `s.substr($o, $l)` with `Int:D $l is copy` ignored the length
(`substr` read its length argument without reading the cell). Both now decont
their operand. This brings P5substr's `t/01-basic.t` from 3 to 18 of 19 tests;
the last one needs `.resume` across a call boundary (#15555).
