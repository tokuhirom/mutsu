use Test;
plan 3;

# Rakudo declares `.Range` only on the numbers (`Int.Range` is the type's
# bounds); a `List`, a `Str` and a `Range` have none (ADR-11276 §9.110).

my @a = 1, 2, 3;
throws-like { @a.Range }, X::Method::NotFound, '.Range on an array';

throws-like { "abcd".Range }, X::Method::NotFound, '.Range on a string';

throws-like { (2..5).Range }, X::Method::NotFound, '.Range on a Range';
