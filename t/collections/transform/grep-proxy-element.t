use Test;

# `.grep` used to drop a `Proxy` element from its result even when the block
# returned True for it. The block WAS invoked and DID see the FETCHed value; the
# loss happened in the result collection.
#
# Root cause: `dispatch_grep` promotes each matched source slot to a shared
# element container so a writeback loop (`for @a.grep(...) { $_++ }`) mutates
# the source, and it re-derived *which* slots matched by scanning the source for
# a value `===` to each result element. A `Proxy` element reaches the result as
# its FETCHed value while the source slot still holds the `Proxy`, so the scan
# never located it -- and since the result is rebuilt from the located slots,
# the element was silently dropped and every `:k`/`:kv`/`:p` key after it
# shifted. The grep loop now reports the matched indices itself.

plan 20;

my $n = 5;
my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
my $l = (1, $p, 3);

# --- the headline repro -----------------------------------------------------
is $l.grep({ True }).elems, 3, 'grep keeps a Proxy element that matches';
is $l.grep({ True }).join(','), '1,5,3', 'the Proxy element reaches the result FETCHed';
is $l.grep({ $_ > 2 }).join(','), '5,3', 'a filtering grep keeps the matching Proxy element';
is $l.grep({ $_ < 2 }).join(','), '1', 'a Proxy element that fails the block is excluded';

# `.map` over the same list already agreed with rakudo; keep it pinned.
is $l.map({ $_ + 1 }).join(','), '2,6,4', 'map over a Proxy element is unchanged';

# --- the adverbs, which shifted by the same miss ----------------------------
is $l.grep({ True }, :k).join(','), '0,1,2', ':k keys are not shifted by a Proxy element';
is $l.grep({ $_ > 2 }, :k).join(','), '1,2', ':k keys of a filtering grep are correct';
is $l.grep({ $_ > 2 }, :kv).join(','), '1,5,2,3', ':kv pairs up the right keys and values';
is $l.grep({ $_ > 2 }, :p).raku, '(1 => 5, 2 => 3).Seq', ':p pairs are correct';

# --- an Array holding a `:=`-bound Proxy element ----------------------------
my @b = 1, 2, 3;
@b[1] := $p;
is @b.grep({ True }).elems, 3, 'grep over an Array with a bound Proxy element keeps it';
is @b.grep({ True }, :k).join(','), '0,1,2', ':k over an Array with a bound Proxy element';

# --- the aliasing this machinery exists for must still work -----------------
my @a = 1, 2, 3, 4;
for @a.grep({ $_ %% 2 }) { $_ *= 10 }
is @a.raku, '[1, 20, 3, 40]', 'grep result still aliases the source slots (writeback)';

# --- equal values must not collapse the index mapping -----------------------
is (7, 7, 7).grep({ True }, :k).join(','), '0,1,2', 'duplicate values keep distinct keys';
is (7, 8, 7).grep({ $_ == 7 }, :k).join(','), '0,2', 'duplicates keyed at their own positions';

# --- shapes that take other code paths --------------------------------------
is (1, 2, 3, 4).grep(-> $x, $y { $x < $y }).raku, '((1, 2), (3, 4)).Seq',
   'a chunked (arity 2) grep is unchanged';
is (1, 'a', 2, 'b').grep(Int).join(','), '1,2', 'smart-match grep is unchanged';
is (1, 'a', 2, 'b').grep(Int, :k).join(','), '0,2', 'smart-match grep :k keys are correct';
is (1..5).grep({ $_ > 3 }, :k).join(','), '3,4', 'grep over a Range reports the right keys';

# --- loop control inside the block ------------------------------------------
is (1..6).grep({ next if $_ == 3; $_ %% 2 }).join(','), '2,4,6', 'next inside a grep block';
is (1..6).grep({ last if $_ == 5; $_ %% 2 }).join(','), '2,4', 'last inside a grep block';

done-testing;
