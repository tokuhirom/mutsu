use Test;

# A smartmatch whose RHS is written as an Int or Num literal numifies the
# topic with `.Numeric(:fail-or-nil)`, as Rakudo compiles it (#16750): a topic
# with no defined `Numeric` dies with X::Multi::NoMatch and a Failure topic
# throws, while the same match against a variable answers False.

plan 16;

my $block = {...};
sub foo { }
class Plain { }
class Numy { method Numeric { 2 } }
class Err is Exception { }

throws-like { $block ~~ 2 }, X::Multi::NoMatch,
    message => /'Numeric(Block:D: :fail-or-nil)'/, 'a Block topic against an Int literal';
throws-like { &foo ~~ 2 }, X::Multi::NoMatch,
    message => /'Numeric(Sub:D: :fail-or-nil)'/, 'a Sub topic';
throws-like { given $block { when 2 { 1 } } }, X::Multi::NoMatch, '`when 2` with a Block topic';
throws-like { Plain.new ~~ -2 }, X::Multi::NoMatch,
    message => /'Numeric(Plain:D: :fail-or-nil)'/, 'a plain object against a negative literal';
throws-like { (a => 1) ~~ 2e0 }, X::Multi::NoMatch, 'a Pair against a Num literal';
throws-like { Mu.new ~~ 0 }, X::Multi::NoMatch, 'a Mu instance';
throws-like { Err.new ~~ 2 }, X::Multi::NoMatch, 'a user exception';
throws-like { Failure.new("gone") ~~ 2 }, X::AdHoc, message => 'gone', 'a Failure topic throws';

my $two = 2;
nok $block ~~ $two, 'against a variable it is just False';
nok $block ~~ 2.5, 'a Rat literal answers False';
nok $block ~~ Inf, '`Inf` is a term, not a numeric literal';
ok Numy.new ~~ 2, 'an object with its own Numeric compares by it';
ok "2" ~~ 2, 'a Str topic numifies as before';
nok "abc" ~~ 2, 'an unnumifiable Str is False';
nok Any ~~ 2, 'a type object is False';
nok X::AdHoc.new ~~ 2, 'a built-in X:: exception keeps its own coercion';
