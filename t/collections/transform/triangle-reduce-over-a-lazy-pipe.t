use v6;
use Test;

# `[\op]` over a lazy PIPE (`(1..*).map(...)`, `.grep(...)`) produced one `Nil`
# per requested element:
#
#     ([\~] (1..*).map(* + 1))[^4]   # was (Nil Nil Nil Nil), raku says (2 23 234 2345)
#
# The scan's source walk (`force_scan_lazy_list`) special-cased every `Range`
# shape and fell through to `value_to_list` for everything else, which declines
# to materialise an unbounded `LazyList` — so the scan stepped `needed` times
# over an EMPTY source. The same walk now pulls exactly the prefix each batch
# needs, through the ordinary bounded lazy pull.
#
# Every expectation below was measured against rakudo 2026.07.

plan 15;

# --- the ticket's repro, both spellings ---------------------------------
is ([\~] (1..*).map(* + 1))[^4].join(' '), '2 23 234 2345',
    'a scan over a map pipe, read by subscript';
is ([\~] (1..*).map(* + 1)).head(4).join(' '), '2 23 234 2345',
    'a scan over a map pipe, read by .head';
is ([\+] (1..*).grep(* %% 2))[^4].join(' '), '2 6 12 20',
    'a scan over a grep pipe';

# --- the shapes that already worked, as invariants ----------------------
is ([\~] 1..*)[^5].join(' '), '1 12 123 1234 12345', 'a scan over a bare infinite range';
is ([\+] 1..*)[^5].join(' '), '1 3 6 10 15', 'the same for [\+]';
is ([\*] 1..*)[^5].join(' '), '1 2 6 24 120', 'the same for [\*]';
is ([\~] <a b c>).join(' '), 'a ab abc', 'a scan over a finite list';
is ([\+] (1, 2, 4 ... *))[^5].join(' '), '1 3 7 15 31', 'a scan over a sequence spec';

# A pipe over a FINITE source must still materialize completely.
is ([\~] (1..5).map(* + 1)).join(' '), '2 23 234 2345 23456',
    'a scan over a finite map pipe materializes';

# The array-assigned spelling stays lazy AND correct (the pin from
# news/2026-09/triangle-reduce-stays-lazy-through-an-array-assignment.md).
my @e = [\~] (1..*).map(* + 1);
is @e[^4].join(' '), '2 23 234 2345', 'the array-assigned spelling';
ok ([\+] (1..*).map(* + 1)).is-lazy, 'the result is still lazy';

# --- and it must stay lazy: the pipe body runs only as far as needed ----
#
# The construction used to pre-compute a 1000-element batch so that eager
# consumers reading the element cache got a prefix. Over a pipe that runs the
# user's closure a thousand times, which is observable — so the pre-compute is
# now skipped for a lazy source and every consumer pulls what it needs.
my $calls = 0;
is ([\+] (1..*).map({ $calls++; $_ }))[^3].join(' '), '1 3 6', 'a counted pipe still answers';
ok $calls <= 100, "the pipe body ran a bounded number of times ($calls)";

my @safe = ([\+] (1..*).map({ die "pulled too far" if $_ > 5; $_ }))[^3];
is @safe.join(' '), '1 3 6', 'a pipe that dies past element 5 is never pulled that far';

my $head-calls = 0;
([\+] (1..*).map({ $head-calls++; $_ })).head(3);
ok $head-calls <= 100, ".head(n) is bounded too ($head-calls)";
