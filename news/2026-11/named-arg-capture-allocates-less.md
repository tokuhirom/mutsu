# Capturing a named argument's container allocates less

`key => $var` as a named argument retains the caller's container (#16002, #16121), and the capture
built a `String` and a `HashSet` in `resolve_alias_root` to walk a sigilless-alias chain that almost
never exists, plus a `format!` for the descriptor name. `resolve_alias_root` now returns a borrowed
name when the variable is not an alias, and the sigiled name is built on the stack. `method-call`
(callgrind, profiling build, same box): 563.7M -> 543.7M instructions and 288.5k -> 228.5k
allocations, behaviour unchanged. #16098 stays open: pre-#16002 was 510.0M / 178.2k.
