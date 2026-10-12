# A nested BEGIN is lifted beside a multi, an `our sub` or an exported sub

A BEGIN nested in a routine ran at compile time only when every routine declared ahead of it in
the same scopes was a plain `sub`; a `multi`, an `our sub` or an `is export` sub kept the scope
blocked, so the BEGIN stayed on its in-place handling and never ran if the routine was not
called. These routines are now only recorded: a BEGIN that does not call any name it cannot copy
is lifted (#10395). One that calls such a routine, or that reaches names dynamically, still stays
behind.
