use Test;

# From the Text::Flags distribution: `constant %h = %hash.Map does Role`
# must keep the role, so its AT-KEY/EXISTS-KEY overrides still dispatch.
plan 6;

role LowerCaseKey {
    method AT-KEY(\key)     { nextwith key.lc }
    method EXISTS-KEY(\key) { nextwith key.lc }
}

my constant %c = do { my %h = ac => 'A'; %h.Map does LowerCaseKey };
is %c<ac>, 'A', 'plain key still works';
is %c{"AC"}, 'A', 'role AT-KEY lowercases the key';
ok %c{"AC"}:exists, 'role EXISTS-KEY lowercases the key';
ok %c ~~ LowerCaseKey, 'constant hash still does the role';

constant %h2 = (:a(1)).Hash does LowerCaseKey;
is %h2.^name, 'Hash+{LowerCaseKey}', 'Hash mixin keeps its type name';
is %h2{"A"}, 1, 'mixed-in Hash AT-KEY dispatches';
