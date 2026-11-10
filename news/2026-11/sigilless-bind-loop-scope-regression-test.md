# Regression test for a loop-body sigilless bind that corrupted the caller's hash

A `my \value = $pair.value` in a loop body, under two enclosing routines that
also name a sigilless `value`, used to overwrite a hash element with the whole
hash (LLM::Chat `t/29-wire-json`). The sigilless param write-back fix
(#17323) already resolved it; this adds the 12-line reduction as a test.
LLM::Chat `t/29-wire-json.rakutest` is now 21/21 like raku.
