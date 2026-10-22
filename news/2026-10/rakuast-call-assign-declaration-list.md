# RakuAST: a `.=` declaration followed by comma list items

`my Bar $c .= new(), ok defined($c), '...'` (as in `roast/S14-roles/basic.t`) no
longer refuses under `MUTSU_RAKUAST=1`. The parser keeps the `.=` declaration
and each further item as sunk statements with no re-read of the variable, so
the loose-tail conversion could not take it apart. The converter now builds the
one comma application rakudo produces, with the `Initializer::CallAssign`
declaration as its first operand; the gist matches rakudo exactly.
`roast/S14-roles/basic.t` passes under the RakuAST frontend.
