# Adjacent colonpairs are a list of all of them

`my @a = :a:!b:42c` is `[:a]` in mutsu (the later pairs were taken as adverbs and dropped, the
legacy frontend's reading, #10257) and `my $c = :a:b` held a bare `:a`. Rakudo's RakuAST frontend,
the oracle, keeps them all: `[:a, :!b, :c(42)]` and `$(:a, :b)`. The parser now yields the list
of every pair, and each value is still evaluated in order. Found by the raku-doc differential
(`Type/Pair.rakudoc:61`).
