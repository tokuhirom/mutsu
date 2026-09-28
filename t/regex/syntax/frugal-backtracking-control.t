use Test;

plan 4;

is ("aab" ~~ / a:? /).Str, 'a', ':? after an unquantified atom is backtracking control';
is ("aab" ~~ / a:! /).Str, 'a', ':! after an unquantified atom remains valid';
is ("aaa" ~~ / a*:? a /).Str, 'a', ':? makes a quantified atom frugal';
is ("abcd" ~~ / :ratchet [ab | abc]:? cd /).Str, 'abcd',
    ':? permits backtracking into an atom under :ratchet';
