use Test;

# A method call's named argument that is a deferred `.map`/`.grep`/gather Seq
# is still read before the callee binds it; every other named argument skips
# that step (the reify pass asks the value's tags first).

plan 6;

class C {
    has @.s;
    has $.k;
    method new(*%_) { self.bless(|%_) }
}
is-deeply C.new(s => (1..3).map(* * 2)).s, [2, 4, 6], 'a mapped Seq fills an @ attribute';
is-deeply C.new(s => gather { take 1; take 2 }).s, [1, 2], 'a gather fills an @ attribute';
is-deeply C.new(s => (1..6).grep(* %% 2)).s, [2, 4, 6], 'a grep Seq fills an @ attribute';
is C.new(k => (1..2).map(* + 1)).k.list, (2, 3), 'a $ attribute keeps the Seq';
my $v = (1..2).map(* * 10);
is C.new(s => $v).s.elems, 1, 'a Seq in a variable is one item';
is-deeply C.new(s => [1, 2], k => 7).s, [1, 2], 'plain arguments';
