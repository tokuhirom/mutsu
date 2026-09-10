use Test;

plan 8;

ok "ab/cd" ~~ m/ab ‘/’ c d/, "curly single quote delimiter in regex atom";
ok "ab/cd" ~~ m/ab ‚/’ c d/, "low-high single quote delimiter in regex atom";
ok "ab/cd" ~~ m/ab ‚/‘ c d/, "low-curly single quote delimiter in regex atom";
ok "ab/cd" ~~ m/ab ｢/｣ c d/, "corner quote delimiter in regex atom";

is "ab/cd" ~~ m/[\w+] +% '/'/, "ab/cd", "ascii single-quoted separator after %";
is "ab/cd" ~~ m/[\w+] +% ‘/’/, "ab/cd", "curly single-quoted separator after %";
is "ab/cd" ~~ m/[\w+] +% ‚/‘/, "ab/cd", "low-curly single-quoted separator after %";
is "ab/cd" ~~ m/[\w+] +% ｢/｣/, "ab/cd", "corner-quoted separator after %";
