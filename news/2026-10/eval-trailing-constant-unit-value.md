# An EVAL'd unit ending in a `constant` has the constant's value

`EVAL('1; constant TD = 8')` returned `1` and `EVAL('my $q = 1; constant TC = 5')` returned `Nil`,
because the trailing `constant` moves whole into the BEGIN prologue and the run-time remainder
ended on an earlier statement. The remainder now ends on a read of the constant, so the unit's
value is the constant's, as in Rakudo (#12000). A trailing `BEGIN` block's value is not covered.
