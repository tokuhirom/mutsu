# Non-trivial proto bodies run as compiled bytecode (ledger §D, multi-dispatch
# VM-ization): a `proto foo($x) { ...; {*} }` with a real body around `{*}` is
# compiled and run like any routine, with `{*}` still redispatching to the
# winning multi candidate. These assertions verify the bypass is byte-identical
# to the interpreter fallback.
use Test;
plan 13;

# basic non-trivial body wraps the dispatched candidate result
proto p1($x) { my $pre = "[$x]"; my $r = {*}; "$pre$r" }
multi p1(Int $x) { "int" }
multi p1(Str $x) { "str" }
is p1(5), "[5]int", "non-trivial body wraps int candidate";
is p1("a"), "[a]str", "non-trivial body wraps str candidate";

# capture-param proto `(|)`
proto p2(|) { "<" ~ {*} ~ ">" }
multi p2(Int) { "i" }
multi p2(Str) { "s" }
is p2(1), "<i>", "capture-param proto body";

# where-constrained candidates dispatched through a non-trivial body
proto p3($x) { "w:" ~ {*} }
multi p3($x where * > 10) { "big" }
multi p3($x) { "small" }
is p3(20), "w:big", "where candidate via non-trivial body";
is p3(2), "w:small", "fallback candidate via non-trivial body";

# callsame from the candidate dispatched under a non-trivial body
proto p4($x) { "pre " ~ {*} }
multi p4(Int $x) { "narrow+" ~ callsame() }
multi p4($x) { "wide" }
is p4(7), "pre narrow+wide", "callsame from candidate under non-trivial body";

# recursion through a non-trivial proto body
proto fib($n) { {*} }
multi fib(0) { 0 }
multi fib(1) { 1 }
multi fib(Int $n) { fib($n - 1) + fib($n - 2) }
is fib(10), 55, "recursion through non-trivial proto body";

# named args through the proto body
proto nm(:$a, :$b) { "n:" ~ {*} }
multi nm(:$a!, :$b!) { "$a-$b" }
is nm(a => 1, b => 2), "n:1-2", "named args through proto body";

# explicit return from the candidate
proto rr($x) { "ignored-prefix"; {*} }
multi rr(Int $x) { return "early"; "unreached" }
is rr(1), "early", "explicit return from candidate";

# proto signature gate still rejects wrong types / accepts right ones
proto pg(Int $x) { "g:" ~ {*} }
multi pg($x) { "ok" }
dies-ok { pg("string") }, "proto signature gate rejects Str";
is pg(3), "g:ok", "proto signature gate passes Int";

# proto body computes on the param before dispatching
proto cc($x) { my $y = $x * 2; "y=$y;" ~ {*} }
multi cc(Int $x) { "got" }
is cc(4), "y=8;got", "proto body computes on param before dispatch";

# side-effecting statement in the body runs before dispatch
my @log;
proto se($x) { @log.push("pre"); my $r = {*}; @log.push("post"); $r }
multi se(Int $x) { @log.push("cand"); "v" }
is se(1) ~ "|" ~ @log.join(","), "v|pre,cand,post", "body statements run around dispatch in order";
