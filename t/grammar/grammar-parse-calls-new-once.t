use Test;

# `.parse` runs its start rule on `self.new`, so a grammar's own `new` runs
# exactly once per parse, from a type object, an instance, or a user `parse`
# that defers to `Grammar`'s with `nextsame` (#13731). Answers checked against
# Rakudo.

plan 5;

grammar H {
    my $n = 0;
    method new(|c) { $n++; callsame }
    token TOP { b }
    method count { $n }
}
my $h = H.new;
is H.count, 1, 'the explicit new';
$h.parse("b");
is H.count, 2, 'an instance parse calls new once';
H.parse("b");
is H.count, 3, 'a type-object parse calls new once';

grammar K { token TOP { b }; method parse(|c) { nextsame } }
grammar K2 is K {
    my $n = 0;
    method new(|c) { $n++; callsame }
    method count { $n }
}
ok K2.parse("b"), 'nextsame reaches Grammar.parse';
is K2.count, 1, 'the deferred parse calls new once';
