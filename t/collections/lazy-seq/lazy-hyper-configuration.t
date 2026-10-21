use Test;

plan 14;

for <hyper race> -> $m {
    my $s = (1..*).map(* + 0)."$m"(:batch(4), :degree(2));
    is $s.is-lazy, False, "lazy .$m is not lazy (HyperSeq.is-lazy)";
    is $s.configuration.raku, 'HyperConfiguration.new(batch => 4, degree => 2)',
        "lazy .$m keeps its configuration";
    is $s.configuration.batch, 4, "lazy .$m batch";
    my $staged = $s.map(* + 1).grep(* %% 2);
    is $staged.configuration.degree, 2, "a stage keeps the configuration of lazy .$m";
    is (1..10)."$m"(:batch(3), :degree(2)).configuration.raku,
        'HyperConfiguration.new(batch => 3, degree => 2)',
        "settled .$m prints its configuration";
}

is (1..*).hyper.configuration.batch, 64, 'default batch of a lazy hyper';
is (1..*).race.configuration.batch, 64, 'default batch of a lazy race';
is (1..*).hyper(:batch(5)).head(3).List, (1, 2, 3), 'a lazy hyper with adverbs streams';
is (1..*).race(:degree(1)).map(* * 2).head(2).List, (2, 4), 'a lazy race with adverbs streams';

done-testing;
