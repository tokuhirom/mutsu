# A TRIR method call on a string or number takes the native lane

A method call inside a typed (TRIR) routine whose receiver is not known at
compile time (`MethodGen`) went through the general by-value method dispatch
every time. JSON::Fast's `parse-numeric` ends in `$str.Numeric`, once per
number, at about 9.7k instructions each.

The VM's own `CallMethod` answers the same call on an immutable scalar (`Str`,
`Int`, `Num`, `Bool`) from its pure native lane, after the same guards: an
augmented method still wins, a junction argument still autothreads, and a
method with its own dispatch branch keeps it. A TRIR site now tries that lane
first and falls back to the general dispatch for anything else.

The 100-record SPDX decode went from 58.46M to 56.67M instructions (−3.1%,
callgrind). `t/vm/codegen/adr8673-trir-scalar-methods.t` checks that TRIR on
and off agree with each other and with rakudo, including a `Failure` from
`.Numeric`, a junction argument and `.raku`.
