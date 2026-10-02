use Test;

plan 10;

sub takes-two() { take 1; take 2 }

# A `gather` result carries a Seq-or-List "view" that a later `.List`/`.list`
# coercion flips without forcing the coroutine. Whichever dispatch opcode the
# eventual forcing method compiles to -- `CallMethod` for an inline receiver,
# `CallMethodMut` for a named-variable one -- must honour that view.

my $inline = (gather takes-two()).List;
is $inline.^name, 'List', '(gather ...).List reports List';
is $inline.raku, '$(1, 2)', '(gather ...).List keeps scalar itemization';

my $g = gather takes-two();
my $viavar = $g.List;
is $viavar.raku, '$(1, 2)', 'the two-statement spelling renders the same';

# `eqv` had no LazyList arm at all, so a gather-backed operand compared False
# without ever running the body.
ok (gather takes-two()).List eqv (1, 2), 'a gather .List is eqv to the equivalent List';
ok (gather { take 3; take 4 }) eqv (3, 4).Seq, 'a bare gather is eqv to the equivalent Seq';
is-deeply (gather takes-two()).List, (1, 2), 'is-deeply over a gather .List';

# A `.map`/`.grep` pipe whose source chain provably bottoms out finite is not
# "lazy enough" to refuse comparison: raku answers eqv rather than throwing.
ok (gather takes-two()).map(* + 1) eqv (gather takes-two()).map(* + 1),
    'eqv between two finite gather pipes answers instead of throwing';
ok (gather takes-two()).map(* + 1) eqv (2, 3).Seq, 'a finite gather pipe is eqv to a Seq';

# A genuinely infinite source still refuses, rather than hanging.
dies-ok { (1, 2 ... Inf) eqv (1, 2 ... Inf) }, 'eqv on two infinite sequences still throws';

# `.cache`'s List view is unaffected.
my $cached = (1, 2, 3).Seq.cache;
ok $cached eqv (1, 2, 3), '.cache still compares eqv to a List';
