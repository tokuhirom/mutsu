use Test;

plan 6;

EVAL q[my \lc = 5; 1];
is (try EVAL q[lc]) // "nv", "nv", "an EVAL's `my \\lc` does not leave `lc` a declared term";
EVAL q[my constant uc = 5; 1];
is (try EVAL q[uc]) // "nv", "nv", "an EVAL's `my constant uc` does not either";
is (try EVAL q[lc("ABC")]) // "nv", "abc", "the builtin routine still answers in a later EVAL";

constant K = 5;
EVAL q[my constant K = 9; 1];
is EVAL(q[K]), 5, "a caller's constant is restored after an EVAL redeclares it";

my \S = 3;
EVAL q[my \S = 4; 1];
is EVAL(q[S]), 3, "... and so is a caller's sigilless term";

EVAL q[constant G = 7; 1];
is (try EVAL q[G]) // "nv", 7, "an `our` constant of the EVAL stays visible";
