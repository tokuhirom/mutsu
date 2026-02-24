use Test;

plan 5;

dies-ok { Supply.grep(* > 0) }, 'Supply.grep as class method dies';

is-deeply Supply.from-list(1..10).grep(* > 5).list, [6, 7, 8, 9, 10],
  'Supply.grep with Callable filters values';

is-deeply Supply.from-list(flat(1..3, "a".."c")).grep(Int).list, [1, 2, 3],
  'Supply.grep with Type filters values';

is-deeply Supply.from-list("a".."z").grep(/<[a..e]>/).list, ["a", "b", "c", "d", "e"],
  'Supply.grep with Regex filters values';

is-deeply Supply.from-list(<foo bar foobar>).grep(/foo/).grep(/bar/).list, ["foobar"],
  'chained Supply.grep filters incrementally';
