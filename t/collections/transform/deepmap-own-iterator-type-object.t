use Test;

# `flatmap`, `deepmap` and `duckmap` read a receiver that brings its own
# `iterator` through it, and treat a type object as one item (#13690).
# Expected values are the `raku` (v2026.09) output.

plan 10;

class PI { method iterator { (4, 5).iterator } }

is PI.new.flatmap({ ($_, $_) }).raku, '(4, 4, 5, 5).Seq', 'flatmap reads the own iterator';
is PI.new.deepmap(* ~ "?").raku, '("4?", "5?")', 'deepmap reads the own iterator';
is PI.new.duckmap(-> Int $x { $x + 1 }).raku, '(5, 6)', 'duckmap reads the own iterator';

is Int.deepmap(-> $x { 42 }).raku, '(42,)', 'deepmap calls the block on a type object';
is Int.duckmap(-> $x { 42 }).raku, '(42,)', 'duckmap calls the block on a type object';
is Array.deepmap(-> $x { 42 }).raku, '[42]', 'Array.deepmap answers an Array';
class A is Array {}
is A.deepmap(-> $x { 42 }).raku, '[42]', '... and so does a subclass of Array';
is Positional.deepmap(-> $x { $x.^name }).raku, '("Positional",)', 'a role type object is one item too';
is [Int, 1].deepmap(-> $x { $x.^name }).raku, '["Int", "Int"]', 'a nested type object is a leaf';
is 42.deepmap(* + 1).raku, '(43,)', 'a plain item still answers a one-item list';
