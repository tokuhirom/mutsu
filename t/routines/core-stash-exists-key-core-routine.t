use Test;

# head-skip-tail's `sub EXPORT` asks `CORE::.EXISTS-KEY('&head')` and exports nothing when
# the core already has the routine. The CORE stash is not materialized, so mutsu answered
# False for every core routine.

plan 8;

ok CORE::.EXISTS-KEY('&head'), 'head is a core routine';
ok CORE::.EXISTS-KEY('&sort'), 'sort';
ok CORE::.EXISTS-KEY('&tail'), 'tail';
ok CORE::.EXISTS-KEY('&say'), 'say';
nok CORE::.EXISTS-KEY('&not-a-routine-at-all'), 'an unknown name is not';
nok CORE::.EXISTS-KEY('say'), 'the key is the full sigiled name';

sub my-own-routine { 1 }
nok CORE::.EXISTS-KEY('&my-own-routine'), 'a routine the program declared is not a core symbol';

ok SETTING::.EXISTS-KEY('&head'), 'SETTING:: answers the same';
