use Test;

plan 12;

# `.first` with `:k`, `:kv` or `:p` pulls only up to the match, as the plain
# form does (rakudo's `first` stops at the first hit whatever the adverb).

class Counting does Iterator {
    has $.n = 0;
    has @.pulled;
    method pull-one { @!pulled.push($!n); $!n < 5 ?? $!n++ !! IterationEnd }
}

for (:k(2), :kv((2, 2)), :p(2 => 2)) -> $adverb {
    my $it = Counting.new;
    is-deeply Seq.new($it).first({ $_ %% 2 && $_ > 0 }, |($adverb.key => True)), $adverb.value,
      "Seq.new(iterator).first(:{$adverb.key}) answers";
    is-deeply $it.pulled.List, (0, 1, 2), "Seq.new(iterator).first(:{$adverb.key}) pulls up to the match";
}

for <k kv p> -> $adverb {
    my @pulled;
    my $seq = gather { for ^5 { @pulled.push($_); take $_ } };
    $seq.first(* == 2, |($adverb => True));
    is-deeply @pulled.List, (0, 1, 2), "gather .first(:$adverb) pulls up to the match";
}

is (1..Inf).map(* * 3).first(* %% 2, :kv), (1, 6), 'an endless Seq answers :kv';
is (gather { take $_ for 1..* }).first(* > 3, :p), 3 => 4, 'an endless gather answers :p';
is-deeply (gather { take $_ for ^3 }).first(* > 7, :k), Nil, 'no match answers Nil';
