use Test;

# The compiled chunk for an embedded regex `{ … }` / `<?{ … }>` body is cached
# per code string and reused at every cursor position (see
# news/2026-09/regex-inline-code-recompiled-per-cursor-position.md). Serving one
# compile many times must not let per-evaluation state survive into the next
# evaluation, nor across two regexes that share the same code text.
#
# The counters below live in a container (`@`/`%`), not a plain scalar: a write
# to an *outer scalar* lexical from inside an assertion does not reach the
# caller's slot yet (todo/tickets/regex-assertion-scalar-write-to-outer-lexical-is-lost.md).

plan 8;

# 1. An assertion is re-evaluated per cursor position, and its own `my`
#    declaration must be re-initialized every time rather than carried over.
my @rounds;
my @stale;
"aaaa" ~~ / [ <?{ my $seen = @rounds.elems; @rounds.push: 1; @stale.push($seen) ; True }> . ]+ /;
is @rounds.elems, 5, "assertion ran once per cursor position";
is @stale.join(','), '0,1,2,3,4', "the assertion's own `my` is re-initialized on every evaluation";

# 2. A plain `{ … }` block writing an outer lexical still reaches the caller
#    after its compile is served from cache (i.e. on the 2nd and later matches).
my $last = '';
for <ab cd ef> -> $s {
    $s ~~ / . . { $last = $/.Str } /;
}
is $last, 'ef', "a cached `{ }` block still writes back to the caller lexical";

# 3. The same code text in two different regexes stays correct in both.
my %n = :count(0);
ok ("xyz" ~~ / x <?{ %n<count>++; True }> y /).so, "assertion true in regex A";
ok ("xqz" ~~ / x <?{ %n<count>++; True }> q /).so, "same assertion text in regex B";
is %n<count>, 2, "each regex evaluated the shared assertion text once";

# 4. `$/` / `$0` bindings are per-evaluation, not baked into the cached chunk.
my @seen;
for <a1 b2 c3> -> $s {
    $s ~~ / (\w) (\d) { @seen.push: ~$0 ~ ~$1 } /;
}
is @seen.join(','), 'a1,b2,c3', "positional captures are re-bound per evaluation";

# 5. An assertion that fails at some positions and succeeds at another is
#    decided per position, not once for the whole match.
my @tries;
ok ("aaaa" ~~ / a <?{ @tries.push: 1; @tries.elems == 3 }> a /).so,
   "assertion decided per cursor position, not once";
