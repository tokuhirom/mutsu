use v6;
use Test;

plan 8;

# An `@` target of a `my (...) :=` signature bind takes a Seq the way an `@`
# parameter does (through its cached List); `my @a := SEQ` is still refused.
my ($s, @b) := (1, (2, 3).map({ $_ * 2 }));
is @b.^name, "List", "an @ target binds a Seq as a List";
is-deeply @b.List, (4, 6), "with the Seq's values";
is @b.elems, 2, "readable more than once";

sub pair-of-seq { "k" => (1, 2).map({ $_ + 1 }) }
my ($key, @vals) := do given pair-of-seq() { .key, .value };
is $key, "k", "the key binds";
is-deeply @vals.List, (2, 3), "the Seq value of a Pair binds to the @ target";

my ($x, $y) := (1, (5, 6).map({ $_ }));
is $y.^name, "Seq", "a scalar target keeps the Seq";

my @plain = 7, 8;
my ($p, @q) := (0, @plain);
@q.push(9);
is-deeply @plain, [7, 8, 9], "an Array element still binds by identity";

throws-like { EVAL 'my @d := (1, 2).map({ $_ }); 1' }, X::TypeCheck::Binding,
    "a plain my @a := Seq is still refused";
