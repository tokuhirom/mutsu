use Test;

# `where`-constrained multi/proto candidates are now VM-dispatched (OTF
# compiled) instead of falling back to the tree-walk interpreter. The winning
# candidate is resolved with `where` evaluated, so the compiled body's binding
# re-check passes; behaviour must stay byte-identical to the interpreter.
# (ledger §D, multi-dispatch VM-ization)

plan 18;

# Same type, distinct where constraints — winner picked by `where`.
multi sub classify(Int $n where * < 0) { "neg" }
multi sub classify(Int $n where * == 0) { "zero" }
multi sub classify(Int $n where * > 0) { "pos" }
is classify(-5), "neg", "where < 0 candidate";
is classify(0), "zero", "where == 0 candidate";
is classify(7), "pos", "where > 0 candidate";

# proto with where candidates.
proto sub describe(|) {*}
multi sub describe($x where { $_ %% 2 }) { "even $x" }
multi sub describe($x where { $_ % 2 }) { "odd $x" }
is describe(4), "even 4", "proto where even";
is describe(3), "odd 3", "proto where odd";

# where referencing a closure variable.
my $limit = 10;
multi sub check(Int $n where { $n < $limit }) { "under" }
multi sub check(Int $n where { $n >= $limit }) { "over" }
is check(3), "under", "where with closure var (under)";
is check(20), "over", "where with closure var (over)";

# nextsame/callsame through where candidates.
proto sub layer(|) {*}
multi sub layer(Int $n where * > 100) { "big " ~ callsame() }
multi sub layer(Int $n) { "base $n" }
is layer(200), "big base 200", "callsame from where candidate";
is layer(3), "base 3", "non-where candidate";

# subset (where under the hood).
subset Even of Int where * %% 2;
multi sub kind(Even $n) { "even" }
multi sub kind(Int $n) { "int" }
is kind(4), "even", "subset Even";
is kind(5), "int", "fall through to Int";

# where referencing an earlier positional param.
multi sub pair-ok($a, $b where { $b > $a }) { "ascending" }
multi sub pair-ok($a, $b) { "other" }
is pair-ok(1, 5), "ascending", "where referencing earlier param";
is pair-ok(5, 1), "other", "earlier-param where fails -> next candidate";

# where on a non-proto multi mixed with plain candidates and recursion.
multi sub fac(Int $n where * <= 1) { 1 }
multi sub fac(Int $n) { $n * fac($n - 1) }
is fac(5), 120, "recursive where multi";

# single sub with passing where runs; failing where dies.
sub only-pos(Int $n where * > 0) { "got $n" }
is only-pos(5), "got 5", "single where sub passes";
dies-ok { only-pos(-1) }, "single where sub dies on failure";

# string-typed where.
multi sub greet(Str $s where *.chars > 3) { "long" }
multi sub greet(Str $s) { "short" }
is greet("hello"), "long", "string where long";
is greet("hi"), "short", "string where short";
