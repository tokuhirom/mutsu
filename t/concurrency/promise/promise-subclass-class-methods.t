use Test;

# Rakudo's Promise class methods build `self.new`, so called on a user
# subclass they answer an instance of it and the subclass's own methods win
# (#14217).

plan 15;

class PRO is Promise { method status { "mine" } }

my $kept = PRO.kept(3);
isa-ok $kept, PRO, "kept answers the subclass";
is $kept.status, "mine", "the subclass's own method wins";
is await($kept), 3, "the promise is still kept with the value";

my $broken = PRO.broken("boom");
isa-ok $broken, PRO, "broken answers the subclass";
is $broken.status, "mine", "its own method wins";
is $broken.cause.message, "boom", "the reason is kept";

is PRO.in(0.01).status, "mine", "in";
is PRO.start({ 42 }).status, "mine", "start";
is await(PRO.start({ 42 })), 42, "start's promise is kept with the block's result";

my $all = PRO.allof(PRO.kept(1), Promise.kept(2));
is $all.status, "mine", "allof (combining subclass instances too)";
await $all;
pass "allof is kept once its inputs are";
is PRO.anyof(PRO.kept(1)).status, "mine", "anyof";

is PRO.kept(5).then({ .result }).result, 5, "then works on the carried promise";
is Promise.kept(2).status, Kept, "Promise itself still answers a plain promise";
isa-ok Promise.kept(2), Promise, "...of the base type";
