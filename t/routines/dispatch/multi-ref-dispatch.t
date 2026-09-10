use v6;
use Test;

# &name of a proto-less multi must multi-dispatch when carried as a value
# into map/grep/first/sort (PLAN §8.17). The dispatcher Sub built by
# resolve_code_var has an empty body; inline fast paths that compile the
# body directly must defer to call_sub_value (sub_is_call_carrier).

plan 14;

multi k(Int $n) { "int $n" }
multi k(Str $s) { "str $s" }

is (1, "a").map(&k).join(" "), "int 1 str a", 'List.map(&multi) dispatches by type';

multi f(Int $n where $n %% 3) { "Fizz" }
multi f(Int $n)               { ~$n }

is (1..5).map(&f).join(" "), "1 2 Fizz 4 5", 'Range.map(&multi) honours where constraints';

my @a = 1, 2, 3, 4, 5, 6;
is @a.map(&f).join(" "), "1 2 Fizz 4 5 Fizz", 'Array.map(&multi) dispatches';

multi even(Int $n) { $n %% 2 }
multi even(Str $s) { False }

is (1, 2, 3, "x", 4).grep(&even).join(","), "2,4", '.grep(&multi) dispatches';
is @a.grep(&even).join(","), "2,4,6", 'Array.grep(&multi) dispatches';
is (1..5).first(&even), 2, '.first(&multi) dispatches';
is (5, 1, 2).first(&even, :end), 2, '.first(&multi, :end) dispatches';

is (map &f, 1..5).join(" "), "1 2 Fizz 4 5", 'map &multi, list (function form)';
is (grep &even, 1..5).join(","), "2,4", 'grep &multi, list (function form)';
is (first &even, 1..5), 2, 'first &multi, list (function form)';

multi neg(Int $n) { -$n }
multi neg(Str $s) { $s }
is (3, 1, 2).sort(&neg).join(","), "3,2,1", '.sort(&multi) dispatches';

multi parity(Int $n) { $n %% 2 ?? "even" !! "odd" }
multi parity(Str $s) { "str" }
is (1..4).classify(&parity).sort(*.key).map({ .key ~ ":" ~ .value.join("|") }).join(" "),
    "even:2|4 odd:1|3", '.classify(&multi) dispatches';

# The captured dispatcher must keep working after the defining scope exits,
# including where-constraint candidates (specificity-sorted capture).
my &saved;
{
    multi g(Int $n where $n %% 3) { "Fizz" }
    multi g(Int $n) { ~$n }
    &saved = &g;
}
is saved(3), "Fizz", 'out-of-scope &multi honours where constraints';
is (1..5).map(&saved).join(" "), "1 2 Fizz 4 5", 'out-of-scope &multi works in map';
