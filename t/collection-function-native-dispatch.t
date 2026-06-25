use Test;

# Pin for the ledger §D(b) slice that dispatches the pure list/coercion builtin
# *functions* (val / list / slip / hash) straight to their native builtin_* impls
# instead of recording a tree-walk function fallback. User subs resolve first, so a
# (block-scoped) user `sub list` still wins.

plan 14;

# val() — allomorph coercion
is val("42").^name, 'IntStr', 'val("42") is IntStr';
is val("42"), 42, 'val("42") numifies to 42';
is val("3.14").^name, 'RatStr', 'val("3.14") is RatStr';
is val("abc").^name, 'Str', 'val("abc") stays Str';
is val("0x1F").^name, 'IntStr', 'val("0x1F") is IntStr';

# list()
is-deeply list(1, 2, 3), (1, 2, 3), 'list(1,2,3)';
is list(1, 2, 3).^name, 'List', 'list returns a List';
is list().elems, 0, 'list() is empty';

# hash()
is hash(a => 1, b => 2)<a>, 1, 'hash from pairs';
is hash("x", 10, "y", 20)<y>, 20, 'hash from flat k/v list';
is hash().elems, 0, 'hash() is empty';

# slip()
is slip(2, 3).^name, 'Slip', 'slip returns a Slip';
{
    my @a = 1, slip(2, 3), 4;
    is-deeply @a, [1, 2, 3, 4], 'slip flattens into surrounding list';
}

# user-defined sub wins over native dispatch
{
    sub list(|c) { "USER-list" }
    is list(9, 9), "USER-list", 'user sub list shadows builtin';
}
