# Slangs by execution, slice 1: a slang's own `statement-control` production runs

A slang whose grammar role adds `token statement-control:sym<twice> { 'twice' <.end-keyword>
<.ws> $<block>=<block> }` with an action that attaches RakuAST is no longer refused at
activation (#13370, ADR-15762). The interpreter that loaded the slang stays alive as the unit's
slang host; where the parser starts a statement it asks the host to run the slang's token and
action, answers the token's `<block>` by parsing the block itself (a back-call over a channel), and
lowers the node the action attached. `twice { say "hi" }` prints twice, as on rakudo 2026.09, and
slang statements nest.

Two engine bugs the repro exposed are fixed: `$<b>=<b>` filed its capture twice (a list of two
Matches instead of one), and a grammar method that answered a Match lost the Match's `.ast`.
