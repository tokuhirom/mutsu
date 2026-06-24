# VM-native `{*}` redispatch (ledger §D, multi-dispatch VM-ization ②③): a
# compiled proto body`s `__PROTO_DISPATCH__()` resolves and runs the winning
# multi candidate as compiled bytecode (`vm_call_proto_dispatch` ->
# `compile_and_call_function_def`), so the candidate body, `nextsame`/`callsame`/
# `samewith`, and nested proto calls all run VM-natively. Byte-identical to raku.
use Test;
plan 7;

# nextsame as tail (replaces dispatch) under a non-trivial proto body
proto a($x) { "A[" ~ {*} ~ "]" }
multi a(Int $x) { nextsame }
multi a($x)     { "any" }
is a(3), "A[any]", "nextsame tail from candidate under non-trivial proto body";

# samewith recursion via candidate
proto c($n) { {*} }
multi c(Int $n) { $n <= 0 ?? "end" !! "c" ~ samewith($n - 1) }
is c(3), "cccend", "samewith recursion under proto";

# callsame value used in candidate arithmetic (returns, composable)
proto d($x) { ">" ~ {*} }
multi d(Int $x where * > 100) { "huge-" ~ callsame() }
multi d(Int $x) { "int-" ~ callsame() }
multi d($x) { "any" }
is d(200), ">huge-int-any", "callsame 3-level chain";
is d(5),   ">int-any",      "callsame 2-level chain (huge skipped)";

# nested proto calls (proto body calls another proto)
proto outer($x) { "O(" ~ inner($x) ~ "|" ~ {*} ~ ")" }
multi outer(Int $x) { "oi" }
proto inner($x) { "I[" ~ {*} ~ "]" }
multi inner(Int $x) { "ii" }
is outer(1), "O(I[ii]|oi)", "nested proto calls";

# proto body + candidate share arg correctly
proto e($x) { my $r = {*}; "$x:$r" }
multi e(Int $x) { $x * $x }
is e(6), "6:36", "proto body and candidate share arg correctly";

# callsame returning a value used in arithmetic
proto f($x) { {*} }
multi f(Int $x) { 1 + callsame() }
multi f($x) { 10 }
is f(5), 11, "callsame value used in candidate arithmetic";
