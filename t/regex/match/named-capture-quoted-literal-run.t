use Test;

plan 6;

my $m = "run x" ~~ /$<key>='run' .*/;
is ~$m<key>, 'run', 'an aliased quoted literal captures the whole run';
is ~("abc" ~~ /$<k>='ab' 'c'/)<k>, 'ab', 'the alias stops at the end of its literal';
is ~("run x" ~~ /$<key>=['run'] .*/)<key>, 'run', 'bracketed form unchanged';
is ~("run" ~~ /$<key>="run"/)<key>, 'run', 'double-quoted literal';
is ("run" ~~ /$<key>='run'/).pos, 3, 'the match covers the literal';
is ~("run" ~~ /$<key>=run/)<key>, 'r', 'a bare word is still per-atom (rakudo)';

# vim: expandtab shiftwidth=4
