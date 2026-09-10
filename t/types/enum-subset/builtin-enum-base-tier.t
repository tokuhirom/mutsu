use Test;

# Built-in enum constants (Order/Endian/ProtocolFamily/Signal) live in the
# process-wide immutable "base" tier of the environment rather than every
# per-frame env overlay (docs/vm-dual-store.md Slice 4b). They must still be
# resolvable by name everywhere a normal lexical would be: bare names,
# qualified names, inside closures, inside threads, and after user code that
# shadows them in an inner scope.

plan 12;

# Bare and qualified names resolve to the same constant.
is Less, Order::Less, "bare Less == Order::Less";
is More, Order::More, "bare More == Order::More";
is Same, Order::Same, "bare Same == Order::Same";

# cmp / <=> produce the Order enum.
is (3 <=> 5), Less, "<=> yields Order::Less";
is (5 <=> 3), More, "<=> yields Order::More";
is ('a' cmp 'a'), Same, "cmp yields Order::Same";

# Signal / Endian constants resolve and carry their numeric value.
is SIGTERM.value, 15, "Signal::SIGTERM numeric value";
ok NativeEndian ~~ Endian, "NativeEndian is an Endian";

# Enum constants are visible inside a closure that captures nothing.
my $cmp = { $^a <=> $^b };
is $cmp(2, 9), Less, "Order enum resolves inside a closure";

# ... and inside threads (the base tier is shared process-wide).
my @r = await (^3).map: -> $i { start { $i <=> 1 } };
is @r.map(*.Str).sort.join(","), "Less,More,Same",
    "Order enum resolves inside threads";

# sort uses <=> returning Order; result is correct.
is (3, 1, 2).sort({ $^a <=> $^b }).join(","), "1,2,3",
    "sort with <=> comparator (Order enum) works";

# A read-only closure over a builtin-enum-returning expression still computes.
sub classify($n) { ($n <=> 0) === Less ?? "neg" !! "nonneg" }
is (classify(-5), classify(5)).join(","), "neg,nonneg",
    "Order enum identity comparison in a sub";
