use Test;

plan 13;

# #13494: .^methods and .^can list the methods Rakudo declares directly on Any,
# Instant, Duration, Range, Seq, Exception and friends that mutsu already serves.
sub names(Mu \t) { t.^methods.map(*.name).Set }

for (Any, <all any one none cache duckmap flatmap slice toggle tree permutations>),
    (Instant, <sin cos sqrt floor ceiling round truncate log exp>),
    (Duration, <sin cos sqrt floor ceiling round truncate log exp>),
    (Range, <first iterator>),
    (Seq, <slice sort>),
    (Exception, <die fail rethrow>) -> ($t, @want) {
    ok @want.all ∈ names($t), "{$t.^name}.^methods lists @want[]";
}

ok Any.^can('all'), 'Any.^can("all")';
ok Instant.^can('sqrt'), 'Instant.^can("sqrt")';

# What is listed is callable.
is-deeply (1, 2).any.so, True, 'Any.any';
is Duration.new(4).sqrt, 2, 'Duration.sqrt';
is (1..3).first(* > 1), 2, 'Range.first';
is (3, 1, 2).Seq.sort.list, (1, 2, 3), 'Seq.sort';

ok Any.^methods.elems >= 60, 'Any.^methods grew';
