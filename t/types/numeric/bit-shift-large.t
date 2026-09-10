use Test;
plan 6;

is (1 +< 65).Str, "36893488147419103232", "left shift promotes to big integer";
is ((1 +< 65) - 1).Str, "36893488147419103231", "large shift arithmetic stays correct";
is (1 +< -1), 0, "negative left shift acts as right shift";
is (-1 +> 65), -1, "large right shift keeps arithmetic sign extension";
is ((1 +< 64) +& ((1 +< 64) - 1)), 0, "bitwise ops work with big integer masks";
is (1 +< 64 - 1).Str, ((1 +< 64) - 1).Str, "shift binds before additive subtraction";
