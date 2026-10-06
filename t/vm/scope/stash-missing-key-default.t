use Test;

plan 7;

is GLOBAL::<$mutsu_stash_missing_11775>.^name, "Any",
    "a missing scalar key in GLOBAL is Any";
is GLOBAL::<&mutsu_stash_missing_11775>.^name, "Any",
    "a missing routine key in GLOBAL is Any";
is PROCESS::<$mutsu_stash_missing_11775>.^name, "Any",
    "a missing scalar key in PROCESS is Any";
is PROCESS::<&mutsu_stash_missing_11775>.^name, "Any",
    "a missing routine key in PROCESS is Any";
is GLOBAL::.AT-KEY('$mutsu_stash_missing_11775').^name, "Any",
    "direct AT-KEY on a Stash uses its Any default";
is MY::<$mutsu_stash_missing_11775>.^name, "Nil",
    "a missing key in a lexical PseudoStash remains Nil";
is MY::.AT-KEY('$mutsu_stash_missing_11775').^name, "Nil",
    "direct AT-KEY on a PseudoStash remains Nil";
