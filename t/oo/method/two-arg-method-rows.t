use Test;

# ADR-11276 §9.113: `substr`, `fmt` and `base` with two arguments are rows
# that take any plain argument, so the deleted `dispatch_2arg` arms' callers
# (a `Whatever`, a `Range`, a separator) reach them.

plan 9;

is "abcdef".substr(1, *), 'bcdef', 'Str.substr($from, *)';
is "abcdef".substr(*-2), 'ef', 'Str.substr(*-2)';
is "abcdef".substr(1..2), 'bc', 'Str.substr(Range)';
is 12345.substr(1, 2), '23', 'Int.substr is Cool.substr';
is (1, 2, 3).fmt("%02d", "-"), '01-02-03', 'List.fmt($format, $separator)';
is (a => 1).Bag.fmt("%s:%s", ","), 'a:1', 'Bag.fmt($format, $separator)';
is 3.5.base(2, *), '11.1', 'Rat.base($radix, *)';
is Duration.new(1.5).base(10, *), '1.5', 'Duration.base($radix, *)';
dies-ok { 42.fmt("%d", ",") }, 'Cool.fmt takes no separator';
