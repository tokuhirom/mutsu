use Test;

plan 6;

is-deeply 42.deepmap(* + 1), (43,), 'deepmap on a single item answers a one-item list';
is-deeply 42.duckmap(-> Int $x { $x + 1 }), (43,), 'duckmap on a single item answers a one-item list';
is-deeply "a".deepmap(*.uc), ("A",), 'deepmap on a Str';
is (1, (2, 3)).flatmap({ ($_, $_) }).List, (1, 1, 2, 3, 2, 3), 'flatmap flattens nested mapper results (.map(...).flat)';
is (1, 2, 3).flatmap({ $_ xx 2 }).List, (1, 1, 2, 2, 3, 3), 'flatmap of a flat mapper result';
is-deeply [1, [2, 3]].deepmap(* + 1), [2, [3, 4]], 'an Array receiver is unchanged';
