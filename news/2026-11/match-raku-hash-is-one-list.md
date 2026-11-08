# `Match.raku` renders named captures as one list of pairs

`Match.raku` wrote `:hash(Map.new((:a(..)), (:b(..))))` with each pair in its
own parentheses; Rakudo (and `Map.raku`) write one list,
`:hash(Map.new((:a(..),:b(..))))`. A match with a single named capture was
already right.
