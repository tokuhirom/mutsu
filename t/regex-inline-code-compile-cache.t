use Test;

# The compiled chunk for an embedded regex `{ … }` / `<?{ … }>` body is cached
# per code string and reused at every cursor position (see
# news/2026-09/regex-inline-code-recompiled-per-cursor-position.md). Serving one
# compile many times must not let per-evaluation state survive into the next
# evaluation, nor across two regexes that share the same code text.
#
# The counters below mostly live in a container (`@`/`%`) for historical
# reasons: a write to an *outer scalar* lexical from inside an assertion used
# not to reach the caller's slot at all (GitHub issue #7593, fixed). The scalar
# forms are pinned here too, since serving a cached compile must not lose the
# per-evaluation writeback either; `t/regex-assertion-outer-scalar-write.t`
# pins the underlying behaviour.

plan 10;

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

# 6. The same, with an outer *scalar* counter: a cached compile must still carry
#    each evaluation's write through to the caller's slot (#7593).
my $scalar-count = 0;
"aaaa" ~~ / [ <?{ $scalar-count++; True }> . ]+ /;
is $scalar-count, 5, "a cached assertion writes an outer scalar once per position";

my $second = 0;
"bbb" ~~ / [ <?{ $second++; True }> . ]+ /;
is $second, 4, "a second regex reusing the cache writes its own outer scalar";
