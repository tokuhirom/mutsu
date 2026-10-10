# Optional callable parameters no longer outrank narrower multi candidates

`multi f(Any:D $_, &c = &say)` counted the `&` sigil as an extra typed parameter even though
it was optional, so it beat `multi f(Bool:D $_, |c)`. Optional parameters are now ignored in
that implied-type narrowness count, as in Rakudo. Found via URI::Query::FromHash, whose
`escape` multis recursed forever.
