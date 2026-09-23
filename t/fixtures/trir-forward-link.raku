# Driver for t/vm/codegen/adr0112-trir-forward-link.t (ADR-0112 Step 1).
# Every call below is from a TRIR routine to one declared AFTER it, which a
# TRIR body can only reach through `CallGen`. Each shape is called several
# times, because the first call links the site and the later ones run through
# the link.
use nqp;

# A forward call with a native `is rw` argument: the write must land.
sub bump-twice(int $p is rw) { late-bump($p); late-bump($p); $p }
sub late-bump(int $p is rw) { $p = nqp::add_i($p, 1); $p }

# Mutual recursion, one direction necessarily forward.
sub is-even(int $n) { nqp::iseq_i($n, 0) ?? 1 !! is-odd(nqp::sub_i($n, 1)) }
sub is-odd(int $n) { nqp::iseq_i($n, 0) ?? 0 !! is-even(nqp::sub_i($n, 1)) }

# A forward call whose callee takes an untyped `$x`: an aggregate argument has
# to take the generic path (it shares its container), a scalar the link.
sub describe($x) { late-describe($x) }
sub late-describe($x) { $x.^name }

for ^3 {
    my int $p = 10;
    say "bump-twice => ", bump-twice($p), " p=", $p;
}
say "is-even(10) => ", is-even(10);
say "is-even(7) => ", is-even(7);
say "is-even(10) again => ", is-even(10);
say "describe => ", (describe(1), describe("s"), describe([1, 2]), describe(1)).join(",");

# A wrapper installed AFTER the site has linked must still be reached.
sub call-late(int $n) { late-target($n) }
sub late-target(int $n) { nqp::add_i($n, 1) }
say "before wrap => ", (call-late(1), call-late(2), call-late(3)).join(",");
&late-target.wrap(-> $n { callsame() * 100 });
say "after wrap => ", (call-late(1), call-late(2)).join(",");
