use v6;
use Test;

# Hash2Class (zef:lizmat) builds X::TypeCheck by hand and matches on its
# message: "expected Str but got Int".

plan 6;

sub msg(\got, \expected) { X::TypeCheck.new(operation => "fetching", :got(got), :expected(expected)).message }

is msg("x", Int), 'Type check failed in fetching; expected Int but got Str ("x")', 'a Str';
is msg(42, Str), 'Type check failed in fetching; expected Str but got Int (42)', 'an Int';
is msg(42, Int), 'Type check failed in fetching; expected Int but got 42', 'a value of the expected type shows alone';
is msg(Str, Int), 'Type check failed in fetching; expected Int but got Str (Str)', 'a type object';
is msg("a" x 40, Int), 'Type check failed in fetching; expected Int but got Str ("aaaaaaaaaaaaaaaaaaa...)', 'a long value is cut';
is msg((a => 1), Str), 'Type check failed in fetching; expected Str but got Pair (:a(1))', 'a Pair';
