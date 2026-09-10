use Test;

plan 5;

# mutsu's Signal enum was missing several members Rakudo exposes (SIGBREAK,
# SIGTRAP, SIGBUS, SIGWINCH, ...), so `Signal.keys.sort[^3]` skipped SIGBREAK.
is Signal.keys.sort[^3].join(' '), 'SIGABRT SIGALRM SIGBREAK',
    'first three signals alphabetically include SIGBREAK';

ok Signal.enums<SIGBREAK>.defined, 'SIGBREAK exists';
ok Signal.enums<SIGWINCH>.defined, 'SIGWINCH exists';

# Existing well-known values are unchanged.
is SIGINT.value, 2, 'SIGINT keeps value 2';
is SIGKILL.value, 9, 'SIGKILL keeps value 9';
