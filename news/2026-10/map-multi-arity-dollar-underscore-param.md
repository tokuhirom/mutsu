# `$_` as a parameter of a multi-parameter map block keeps its own value

`(1, [3]).kv.map(-> $n, $_ { ... })` bound the block's parameters and then
topicalized the first element of each chunk over them, so the `$_` parameter
read the first element (`$n`) and `when Positional` never matched. The topic is
now set first and the parameters are bound after it, in the map, grep and
writeback loops. Red's `red-defaults` (`%drivers.kv.map(-> $name, $_ { when
Capture|Positional { ... } })`) hit this (#14094).
