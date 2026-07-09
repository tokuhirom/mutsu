use v6;
use Test;

plan 8;

# Pin for the stale pending_call_arg_sources leak: inside a Rust-driven .map
# loop, the body's own method-call opcode left its arg-source names behind,
# and the NEXT iteration's sigilless param bind re-resolved its args by those
# names against leaked env keys — so List-valued chunks repeated the first
# iteration's values (S32-hash/multislice-6e.t block 3).

# method-call argument position (the original failing shape)
is-deeply ((1,2),"A",(3,4),"B").map(-> \k, \v { Pair.new(k,v) }).List,
  ((1,2) => "A", (3,4) => "B"),
  'sigilless List params stay fresh across map iterations (method call args)';

# both params List-valued
is-deeply ((1,2),(9,9),(3,4),(8,8)).map(-> \k, \v { Pair.new(k,v) }).List,
  ((1,2) => (9,9), (3,4) => (8,8)),
  'both List-valued sigilless params stay fresh';

# swapped argument order
is-deeply ((1,2),"A",(3,4),"B").map(-> \k, \v { Pair.new(v,k) }).List,
  ("A" => (1,2), "B" => (3,4)),
  'swapped arg order stays fresh';

# assignment into a local then return
is-deeply ((1,2),(9,9),(3,4),(8,8)).map(-> \k, \v { my $p = Pair.new(k,v); $p }).List,
  ((1,2) => (9,9), (3,4) => (8,8)),
  'via intermediate local';

# function-call argument position
sub id($a, $b) { ($a, $b) }
is-deeply ((1,2),(9,9),(3,4),(8,8)).map(-> \k, \v { id(k, v) }).List,
  (((1,2),(9,9)), ((3,4),(8,8))),
  'user sub call args stay fresh';

# builtin function argument position
is-deeply ((1,2),(9,9),(3,4),(8,8)).map(-> \k, \v { max(k, v) }).List,
  ((9,9), (8,8)),
  'builtin call args stay fresh';

# grep sees fresh values too
is-deeply ((1,2),(9,9),(3,4),(8,8)).grep(-> \k, \v { k[0] > 2 }).List,
  (((3,4),(8,8)),),
  'grep sigilless chunk stays fresh';

# plain list body (control case)
is-deeply ((1,2),(9,9),(3,4),(8,8)).map(-> \k, \v { (k, v) }).List,
  (((1,2),(9,9)), ((3,4),(8,8))),
  'plain list body control case';

# vim: expandtab shiftwidth=4
